package org.jetbrains.jps.incremental.scala.sources

import org.jetbrains.jps.ModuleChunk
import org.jetbrains.jps.incremental.scala.SourceDependenciesProviderService
import org.jetbrains.jps.model.module.{JpsModule, JpsModuleDependency}

import scala.jdk.CollectionConverters._

class SharedSourceDependenciesProviderService extends SourceDependenciesProviderService {
  override def getSourceDependenciesFor(chunk: ModuleChunk): Seq[JpsModule] = {
    val modules = chunk.getModules.asScala.toSeq

    val dependencies = modules.flatMap(_.getDependenciesList.getDependencies.asScala)

    // should be in sync with org.jetbrains.sbt.project.settings.SbtProjectSettings.separateProdAndTestSources
    val prodTestSourcesSeparated = Option(System.getProperty("sbt.prod.test.separated")).flatMap(_.toBooleanOption).getOrElse(false)

    val isTargetMain = isMainModule(chunk.representativeTarget().getModule.getName)
    val isSuitableName: JpsModule => Boolean = { jpsModule =>
      // note: when modules are separated to main/test in projects that contain shared sources its test module will have dependencies
      // to shared sources test module and shared sources main module. Because of this, we cannot treat shared sources main module as a source dependency for test module.
      // To decide what shared sources module should be taken into account we are checking module names suffixes - main modules should add shared sources main modules,
      // and the same with test.
      val isJpsModuleMain = isMainModule(jpsModule.getName)
      !prodTestSourcesSeparated || (isTargetMain.nonEmpty && isTargetMain == isJpsModuleMain)
    }

    dependencies.collect {
      case it: JpsModuleDependency if {
        val module = it.getModule
        module != null && module.getModuleType == SharedSourcesModuleType.INSTANCE && isSuitableName(module)
      } => it.getModule
    }
  }

  /**
   * Returns:
   * <ul>
   * <li>Some(true) - if module is main module</li>
   * <li>Some(false) - if module is test module</li>
   * <li>None - if it is not source set module</li>
   * </ul>
   */
  private def isMainModule(moduleName: String): Option[Boolean] = {
    val mainPattern = """.*\.main(~[\d]+)?$""".r
    val testPattern = """.*\.test(~[\d]+)?$""".r
    if (mainPattern.matches(moduleName)) Some(true)
    else if (testPattern.matches(moduleName)) Some(false)
    else None
  }
}
