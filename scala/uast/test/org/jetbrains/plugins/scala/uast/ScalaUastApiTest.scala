package org.jetbrains.plugins.scala.uast

import com.intellij.uast.UastHintedVisitorAdapter
import org.jetbrains.plugins.scala.extensions.{ObjectExt, PsiElementExt, inReadAction}
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScCaseClauses
import org.jetbrains.plugins.scala.lang.psi.uast.withPossibleSourceTypesCheck
import org.jetbrains.plugins.scala.uast.AbstractUastFixtureTest._
import org.jetbrains.uast.visitor.{AbstractUastNonRecursiveVisitor, AbstractUastVisitor}
import org.jetbrains.uast.{UAnnotation, UElement, UFile, ULambdaExpression, ULiteralExpression}
import org.junit.Assert

import scala.jdk.CollectionConverters._
import scala.language.postfixOps

class ScalaUastApiTest extends AbstractUastFixtureTest {
  override def check(testName: String, file: UFile): Unit = {}

  import ScalaUastApiTest._

  def testUastAnchors(): Unit =
    doTest("SimpleClass", { (_, file) =>
      val uClass = file.getClasses.asScala.filter(_.getQualifiedName == "SimpleClass").ensuring(_.size == 1).head
      Assert.assertEquals("SimpleClass", uClass.getUastAnchor ?> (_.getSourcePsi) ?> (_.getText) orNull)

      val uMethod = uClass.getMethods.filter(_.getName == "bar").ensuring(_.length == 1).head
      Assert.assertEquals("bar", uMethod.getUastAnchor ?> (_.getSourcePsi) ?> (_.getText) orNull)

      val uParameter = uMethod.getUastParameters.asScala.filter(_.getName == "param").ensuring(_.length == 1).head
      Assert.assertEquals("param", uParameter.getUastAnchor ?> (_.getSourcePsi) ?> (_.getText) orNull)
    })

  def testAnnotationAnchor(): Unit =
    doTest("SimpleClass", { (_, file) =>
      val uAnnotation = findElementByText[UAnnotation](file, "@java.lang.Deprecated")
      Assert.assertEquals("Deprecated", uAnnotation.getUastAnchor ?> (_.getSourcePsi) ?> (_.getText) orNull)
    })

  def testStringLiteral(): Unit =
    doTest("Annotations", { (_, file) =>
      val literal1 = findElementByTextFromPsi[ULiteralExpression](file, "\"abc\"")
      Assert.assertTrue(literal1.isString)
      Assert.assertEquals("abc", literal1.getValue)

      val literal2 = findElementByTextFromPsi[ULiteralExpression](file, "123")
      Assert.assertFalse(literal2.isString)
      Assert.assertEquals(123, literal2.getValue)
    })

  def test_SCL20739(): Unit = {
    val psiFile = myFixture.configureByText(
      "Foo.scala",
      s"""
         |object Foo {
         |  1 match { case _ => ${CARET}_ }
         |}
         |""".stripMargin
    )

    val underscoreLeafPsi =
      psiFile.findElementAt(inReadAction(myFixture.getCaretOffset))

    val psiVisitor = UastHintedVisitorAdapter.create(
      psiFile.getLanguage,
      new AbstractUastNonRecursiveVisitor {
        override def visitElement(element: UElement): Boolean = {
          element.accept(new AbstractUastVisitor {})
          super.visitElement(element)
        }
      },
      Array(classOf[ULambdaExpression])
    )

    withPossibleSourceTypesCheck {
      inReadAction {
        underscoreLeafPsi.withParents
          .takeWhile(!_.is[ScCaseClauses])
          .foreach(_.accept(psiVisitor))
      }
    }
  }
}

object ScalaUastApiTest {
  // Kotlin null-propagation style improves readability
  implicit class OptionOps[A](val o: Option[A]) extends AnyVal {
    /**
     * Imitates Kotlin `?.` safe call operator.
     * Unlike {{{Option.map}}} method result of applying a transforming function
     * can be null itself so it is wrapped into [[Option]] too.
     */
    def ?>[B](f: A => B): Option[B] = o.flatMap(v => Option(f(v)))
  }

  implicit class AutoOptionWrapper[A](val v: A) extends AnyVal {
    def ?>[B](f: A => B): Option[B] = Option(v) ?> f
  }
}
