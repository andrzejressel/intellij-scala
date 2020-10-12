package org.jetbrains.plugins.scala.compilation

import com.intellij.openapi.application.ex.{ApplicationEx, ApplicationManagerEx}
import com.intellij.openapi.projectRoots.Sdk
import com.intellij.openapi.util.registry.{Registry, RegistryValue}
import com.intellij.util.xmlb.XmlSerializerUtil
import org.jetbrains.plugins.scala.compiler.ScalaCompileServerSettings
import org.jetbrains.plugins.scala.externalHighlighters.ScalaHighlightingMode

import scala.util.Try

// TODO: move it to `scala` package and rename to some more generic utility class
object CompilerTestUtil {

  trait RevertableChange {

    // TODO: rename: add "Change" suffux
    def apply(): Unit
    def revert(): Unit

    final def apply(body: => Any): Unit =
      run(body)

    final def run(body: => Any): Unit = {
      this.apply()
      try
        body
      finally
        this.revert()
    }

    final def |+| (change: RevertableChange): RevertableChange = {
      val changes = this match {
        case composite: CompositeRevertableChange => composite.changes :+ change
        case _                                    => Seq(this, change)
      }
      new CompositeRevertableChange(changes)
    }
  }

  object NoOpRevertableChange extends RevertableChange {
    override def apply(): Unit = ()
    override def revert(): Unit = ()
  }

  class CompositeRevertableChange(val changes: Seq[RevertableChange]) extends RevertableChange {
    override def apply(): Unit = changes.foreach(_.apply())
    override def revert(): Unit = changes.reverse.foreach(_.revert())
  }

  private def compileServerSettings: ScalaCompileServerSettings =
    ScalaCompileServerSettings.getInstance().ensuring(
      _ != null,
      "could not get instance of compileServerSettings. Was plugin artifact built before running test?"
    )

  def withModifiedCompileServerSettings(body: ScalaCompileServerSettings => Unit): RevertableChange = new RevertableChange {
    private var settingsBefore: ScalaCompileServerSettings = _
    private lazy val settings: ScalaCompileServerSettings = compileServerSettings

    override def apply(): Unit = {
      settingsBefore = XmlSerializerUtil.createCopy(settings)
      body(settings)
    }

    override def revert(): Unit =
      XmlSerializerUtil.copyBean(settingsBefore, settings)
  }

  def withEnabledCompileServer(enable: Boolean): RevertableChange = new CompositeRevertableChange(Seq(
    withModifiedCompileServerSettings { settings =>
      settings.COMPILE_SERVER_ENABLED = enable
      settings.COMPILE_SERVER_SHUTDOWN_IDLE = true
      settings.COMPILE_SERVER_SHUTDOWN_DELAY = 30
    },
    new RevertableChange {
      private var saveAllowedBefore: Boolean = _
      private lazy val application: ApplicationEx = ApplicationManagerEx.getApplicationEx

      override def apply(): Unit = {
        saveAllowedBefore = application.isSaveAllowed
        application.setSaveAllowed(true)
        application.saveSettings()
      }

      override def revert(): Unit = {
        application.setSaveAllowed(saveAllowedBefore)
        application.saveSettings()
      }
    }
  ))

  def withForcedJdkForBuildProcess(jdk: Sdk): RevertableChange = new RevertableChange {
    private var jdkBefore: Option[String] = None

    override def apply(): Unit = {
      jdk.getHomeDirectory match {
        case null =>
          throw new RuntimeException(s"Failed to set up JDK, got: $jdk")
        case homeDirectory =>
          val jdkHome = homeDirectory.getCanonicalPath
          val registry = Registry.get("compiler.process.jdk")
          jdkBefore = Try(registry.asString).toOption
          registry.setValue(jdkHome)
      }
    }

    override def revert(): Unit =
      jdkBefore.foreach { jdk =>
        Registry.get("compiler.process.jdk").setValue(jdk)
      }
  }

  def withCompileServerJdk(sdk: Sdk): RevertableChange =
    withModifiedCompileServerSettings { settings =>
      settings.USE_DEFAULT_SDK = false
      settings.COMPILE_SERVER_SDK = sdk.getName
    }

  private def withModifiedRegistryValueInternal[A](key: String,
                                                   newValue: A,
                                                   getter: RegistryValue => A,
                                                   setter: (RegistryValue, A) => Unit): RevertableChange =
    new RevertableChange {
      private var before: Option[A] = None

      override def apply(): Unit = {
        val registryValue = Registry.get(key)
        before = Some(getter(registryValue))
        setter(registryValue, newValue)
      }

      override def revert(): Unit =
        before.foreach { oldValue =>
          setter(Registry.get(key), oldValue)
        }
    }

  def withModifiedRegistryValue(key: String, newValue: Boolean): RevertableChange =
    withModifiedRegistryValueInternal[Boolean](key, newValue, _.asBoolean, _ setValue _)

  def withModifiedRegistryValue(key: String, newValue: Int): RevertableChange =
    withModifiedRegistryValueInternal[Int](key, newValue, _.asInteger(), _ setValue _)

  def withErrorsFromCompiler(body: => Unit): Unit = {
    val newValue = true
    val revertable = withModifiedRegistryValue(ScalaHighlightingMode.ShowScalacErrorsKey, newValue) |+|
      withModifiedRegistryValue(ScalaHighlightingMode.ShowDotcErrorsKey, newValue)
    revertable.run(body)
  }
}
