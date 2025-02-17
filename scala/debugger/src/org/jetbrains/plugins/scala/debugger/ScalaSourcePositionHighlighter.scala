package org.jetbrains.plugins.scala.debugger

import com.intellij.debugger.SourcePosition
import com.intellij.debugger.engine.SourcePositionHighlighter
import com.intellij.openapi.util.TextRange
import com.intellij.psi.{PsiElement, PsiMethod}
import com.intellij.util.DocumentUtil
import org.jetbrains.plugins.scala.ScalaLanguage
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScEarlyDefinitions
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.ScTemplateBody
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScTypeDefinition}
import org.jetbrains.plugins.scala.util.AnonymousFunction

class ScalaSourcePositionHighlighter extends SourcePositionHighlighter {
  override def getHighlightRange(sourcePosition: SourcePosition): TextRange = {
    if (isScalaLanguage(sourcePosition)) {
      val element = sourcePosition.getElementAt
      if (element eq null) return null

      val document = sourcePosition.getFile.getViewProvider.getDocument
      if (document eq null) return null

      val lineRange = DocumentUtil.getLineTextRange(document, sourcePosition.getLine)

      if (isWholeLine(lineRange, element)) return null

      containingLambda(lineRange, element).map(_.getTextRange).orNull
    }
    else null
  }

  private def isScalaLanguage(sourcePosition: SourcePosition): Boolean =
    sourcePosition.getFile.getLanguage.isKindOf(ScalaLanguage.INSTANCE)

  private def isWholeLine(lineRange: TextRange, element: PsiElement): Boolean =
    lineRange == element.getTextRange

  private def isContainedOnLine(lineRange: TextRange)(element: PsiElement): Boolean =
    lineRange.contains(element.getTextRange)

  private def containingLambda(lineRange: TextRange, element: PsiElement): Option[PsiElement] =
    element.withParentsInFile.takeWhile(isContainedOnLine(lineRange)).collectFirst {
      case e if AnonymousFunction.isGenerateAnonfun211(e) => Some(e)
      case _: PsiMethod => None
      case _: ScTemplateBody => None
      case _: ScEarlyDefinitions => None
      case _: ScTypeDefinition => None
    }.flatten.filterNot(ScalaPositionManager.isInsideMacro)
}
