package org.jetbrains.plugins.scala.worksheet.actions.topmenu

import com.intellij.icons.AllIcons
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent}
import com.intellij.openapi.application.ModalityState
import com.intellij.openapi.compiler.{CompileContext, CompileStatusNotification, CompilerManager}
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.fileEditor.FileEditorManager
import com.intellij.openapi.keymap.{KeymapManager, KeymapUtil}
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.{PsiDocumentManager, PsiFile}
import javax.swing.Icon
import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.extensions.{inWriteAction, invokeAndWait, invokeLater, LoggerExt}
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.statistics.{FeatureKey, Stats}
import org.jetbrains.plugins.scala.worksheet.actions.WorksheetFileHook
import org.jetbrains.plugins.scala.worksheet.processor.WorksheetCompiler
import org.jetbrains.plugins.scala.worksheet.processor.WorksheetCompiler.WorksheetCompilerResult
import org.jetbrains.plugins.scala.worksheet.processor.WorksheetCompiler.WorksheetCompilerResult.WorksheetCompilerError
import org.jetbrains.plugins.scala.worksheet.runconfiguration.WorksheetCache
import org.jetbrains.plugins.scala.worksheet.settings.{WorksheetCommonSettings, WorksheetFileSettings}

import scala.concurrent.{ExecutionContext, Future, Promise}

class RunWorksheetAction extends AnAction with TopComponentAction {

  override def genericText: String = ScalaBundle.message("worksheet.execute.button")

  override def actionIcon: Icon = AllIcons.Actions.Execute

  override def shortcutId: Option[String] = Some("Scala.RunWorksheet")

  override def actionPerformed(e: AnActionEvent): Unit =
    RunWorksheetAction.runCompilerForSelectedEditor(e, auto = false)

  override def update(e: AnActionEvent): Unit = {
    super.update(e)

    val shortcuts = KeymapManager.getInstance.getActiveKeymap.getShortcuts("Scala.RunWorksheet")

    if (shortcuts.nonEmpty) {
      val shortcutText = s" (${KeymapUtil.getShortcutText(shortcuts(0))})"
      e.getPresentation.setText(ScalaBundle.message("worksheet.execute.button") + shortcutText)
    }
  }
}

object RunWorksheetAction {

  private val Log: Logger = Logger.getInstance(getClass)

  sealed trait RunWorksheetActionResult
  object RunWorksheetActionResult {
    object Done extends RunWorksheetActionResult
    sealed trait Error extends RunWorksheetActionResult
    object NoModuleError extends Error
    object NoWorksheetFileError extends Error
    object AlreadyRunning extends Error
    final case class ProjectCompilationError(aborted: Boolean, errors: Int, warnings: Int, context: CompileContext) extends Error
    final case class WorksheetRunError(error: WorksheetCompilerError) extends Error
  }

  def runCompilerForSelectedEditor(e: AnActionEvent, auto: Boolean): Unit = {
    val project = e.getProject match {
      case null    => return
      case project => project
    }
    runCompilerForSelectedEditor(project, auto)
  }

  def runCompilerForSelectedEditor(project: Project, auto: Boolean): Unit = {
    Stats.trigger(FeatureKey.runWorksheet)

    val editor = FileEditorManager.getInstance(project).getSelectedTextEditor
    if (editor == null) return

    runCompiler(project, editor, auto)
  }

  def runCompiler(project: Project, editor: Editor, auto: Boolean): Future[RunWorksheetActionResult] = {
    Log.debugSafe(s"worksheet evaluation started")
    val promise = Promise[RunWorksheetActionResult]()
    try {
      doRunCompiler(project, editor, auto)(promise)
    } catch {
      case ex: Exception =>
        promise.failure(ex)
    }
    val runImmediatelyExecutionContext = new ExecutionContext {
      override def execute(runnable: Runnable): Unit = runnable.run()
      override def reportFailure(cause: Throwable): Unit = Log.error(cause)
    }
    val future = promise.future
    future.onComplete(s => Log.debugSafe("worksheet evaluation result: " + s.toString))(runImmediatelyExecutionContext)
    future
  }

  private def doRunCompiler(project: Project, editor: Editor, auto: Boolean)
                           (promise: Promise[RunWorksheetActionResult]): Unit = {

    val psiFile: PsiFile = PsiDocumentManager.getInstance(project).getPsiFile(editor.getDocument)
    val vFile = psiFile.getVirtualFile

    Log.debugSafe(s"worksheet file: ${vFile.getPath}")

    val file: ScalaFile = psiFile match {
      case file: ScalaFile if file.isWorksheetFile => file
      case _ =>
        promise.success(RunWorksheetActionResult.NoWorksheetFileError)
        return
    }

    val fileSettings = WorksheetCommonSettings(file)

    val module: Module = fileSettings.getModuleFor match {
      case m: Module => m
      case _ =>
        promise.success(RunWorksheetActionResult.NoModuleError)
        return
    }

    doRunCompiler(project, editor, auto, vFile, file, fileSettings.isMakeBeforeRun, module)(promise)
  }

  private def doRunCompiler(project: Project, editor: Editor, auto: Boolean,
                            vFile: VirtualFile, file: ScalaFile, makeBeforeRun: Boolean, module: Module)
                           (promise: Promise[RunWorksheetActionResult]): Unit = {
    val viewer = WorksheetCache.getInstance(project).getViewer(editor)
    if (viewer != null && !WorksheetFileSettings.isRepl(file)) {
      invokeAndWait(ModalityState.any()) {
        inWriteAction {
          CleanWorksheetAction.resetScrollModel(viewer)
          if (!auto) {
            CleanWorksheetAction.cleanWorksheet(file.getNode, editor, viewer, project)
          }
        }
      }
    }

    RunWorksheetAction.synchronized {
      if (WorksheetFileHook.isRunning(vFile)) {
        promise.success(RunWorksheetActionResult.AlreadyRunning)
        return
      } else {
        invokeAndWait(WorksheetFileHook.disableRun(vFile, None))
      }
    }

    def runnable(): Unit = {
      val callback: WorksheetCompilerResult => Unit = result => {
        val resultTransformed = result match {
          case WorksheetCompilerResult.CompiledAndEvaluated => RunWorksheetActionResult.Done
          case error: WorksheetCompilerError                => RunWorksheetActionResult.WorksheetRunError(error)
        }
        promise.success(resultTransformed)

        val hasErrors = resultTransformed != RunWorksheetActionResult.Done
        invokeLater {
          WorksheetFileHook.enableRun(vFile, hasErrors)
        }
      }
      val compiler = new WorksheetCompiler(module, editor, file, callback, auto)
      compiler.compileAndRunFile()
    }

    if (makeBeforeRun) {
      val compilerNotification: CompileStatusNotification =
        (aborted: Boolean, errors: Int, warnings: Int, context: CompileContext) => {
          val finishedWithError = aborted && errors != 0
          if (!finishedWithError) {
            runnable()
          } else {
            promise.success(RunWorksheetActionResult.ProjectCompilationError(aborted, errors, warnings, context))
          }
        }
      CompilerManager.getInstance(project).make(module, compilerNotification)
    } else {
      runnable()
    }
  }
}