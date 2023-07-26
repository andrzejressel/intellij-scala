package org.jetbrains.plugins.scala.highlighter.usages

import com.intellij.codeInsight.highlighting.HighlightUsagesHandlerBase
import com.intellij.openapi.editor.Editor
import com.intellij.psi.search.LocalSearchScope
import com.intellij.psi.{PsiClass, PsiElement, PsiFile}
import com.intellij.util.Consumer
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.findUsages.factory.{ScalaFindUsagesConfiguration, ScalaFindUsagesHandler}
import org.jetbrains.plugins.scala.lang.psi.api.base.{Constructor, ScConstructorInvocation, ScReference, ScStableCodeReference}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScEnum
import org.jetbrains.plugins.scala.lang.resolve.ScalaResolveResult

import java.util

class ScalaHighlightConstructorInvocationUsages(reference: Option[ScReference], file: PsiFile, editor: Editor)
  extends HighlightUsagesHandlerBase[PsiElement](editor, file)
{

  def this(invoc: ScConstructorInvocation, file: PsiFile, editor: Editor) = this(invoc.reference, file, editor)

  private val elementsToHighlight = reference
    .flatMap(_.bind())
    .collect {
      case ScalaResolveResult(clazz: PsiClass, _) => (clazz, None)
      case ScalaResolveResult(Constructor(constructor), _) =>
        val cls = constructor.containingClass match {
          case ScEnum.Original(enum) => enum
          case cls                   => cls
        }
        (cls, Some(constructor))
    }

  override def getTargets: util.List[PsiElement] = reference.fold(util.Collections.emptyList[PsiElement])(util.Collections.singletonList)

  override def selectTargets(targets: util.List[_ <: PsiElement], selectionConsumer: Consumer[_ >: util.List[_ <: PsiElement]]): Unit =
    selectionConsumer.consume(targets)

  override protected def addOccurrence(element: PsiElement): Unit = {
    if (element != null && element.getContainingFile == file)
      super.addOccurrence(element match {
        case ref: ScStableCodeReference => ref.nameId
        case e => e
      })
  }

  override def computeUsages(targets: util.List[_ <: PsiElement]): Unit = elementsToHighlight.foreach { case (classToHighlight, constructor) =>
    val project = file.getProject
    val config = ScalaFindUsagesConfiguration.getInstance(project)
    val manager = new ScalaFindUsagesHandler(classToHighlight, config)
    val localSearchScope = new LocalSearchScope(file)

    manager
      .findReferencesToHighlight(classToHighlight, localSearchScope)
      .forEach(e => addOccurrence(e.getElement))
    addOccurrence(classToHighlight.getNameIdentifier)
    constructor
      .flatMap(_.getNameIdentifier.toOption)
      .filter(_.textMatches("this"))
      .foreach(addOccurrence)
  }
}
