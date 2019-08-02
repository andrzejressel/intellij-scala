package org.jetbrains.plugins.scala.lang.typeInference

import org.jetbrains.plugins.scala.DependencyManagerBase._
import org.jetbrains.plugins.scala.PerfCycleTests
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import org.jetbrains.plugins.scala.debugger.{ScalaVersion, Scala_2_12}
import org.jetbrains.plugins.scala.project._
import org.jetbrains.plugins.scala.project.settings.ScalaCompilerConfiguration
import org.junit.experimental.categories.Category

@Category(Array(classOf[PerfCycleTests]))
class PartialUnificationCatsHighlightingTest extends ScalaLightCodeInsightFixtureTestAdapter {
  override implicit val version: ScalaVersion = Scala_2_12

  override def librariesLoaders: Seq[LibraryLoader] =
    super.librariesLoaders :+ IvyManagedLoader("org.typelevel" %% "cats-core" % "1.4.0")

  override def setUp(): Unit = {
    super.setUp()
    getModule.scalaCompilerSettings.additionalCompilerOptions = Seq("-Ypartial-unification")

    val defaultProfile = ScalaCompilerConfiguration.instanceIn(getProject).defaultProfile
    val newSettings = defaultProfile.getSettings
    newSettings.plugins = newSettings.plugins :+ "kind-projector"
    defaultProfile.setSettings(newSettings)
  }

  def testEitherSequence(): Unit = checkTextHasNoErrors(
    """
      |import cats.implicits._
      |
      |val x: Either[String, Option[Either[String, Int]]] = Right(Some(Right(1)))
      |val y = x.flatMap(_.sequence)
      |val z = x.flatMap(_.traverse(identity))
    """.stripMargin
  )

  def testSCL14919(): Unit = checkTextHasNoErrors(
    """
      |import cats._
      |import cats.implicits._
      |object A {
      |  val m: Map[String, Option[Int]] = Map(
      |    "1" -> 1.some,
      |    "2" -> 2.some
      |  )
      |  m.unorderedSequence
      |}
    """.stripMargin
  )

  def testSCL16007(): Unit = checkTextHasNoErrors(
    """
      |import cats.{~>, Functor}
      |import cats.data.EitherK
      |import cats.instances.option._
      |
      |trait HFunctor[H[_[_], _]] {
      |  def map[F[_] : Functor, A, B](hfa: H[F, A])(f: A => B): H[F, B]
      |  def hmap[F[_], G[_], A](hfa: H[F, A])(f: F ~> G): H[G, A]
      |}
      |object HFunctor {
      |  def apply[H[_[_], _] : HFunctor]: HFunctor[H] = implicitly
      |  object ops {
      |    implicit class HFunctorSyntax[H[_[_], _], F[_], A](val hfa: H[F, A]) extends AnyVal {
      |      def map[B](f: A => B)(implicit H: HFunctor[H], F: Functor[F]): H[F, B] = H.map(hfa)(f)
      |      def hmap[G[_]](f: F ~> G)(implicit H: HFunctor[H]): H[G, A] = H.hmap(hfa)(f)
      |    }
      |  }
      |}
      |
      |object Test {
      |  import HFunctor.ops._
      |  implicit def eitherKHFunctor[J[_] : Functor]: HFunctor[EitherK[J, *[_], *]] = new HFunctor[EitherK[J, *[_], *]] {
      |    override def map[F[_] : Functor, A, B](hfa: EitherK[J, F, A])(f: A => B): EitherK[J, F, B] = hfa.map(f)
      |    override def hmap[F[_], G[_], A](hfa: EitherK[J, F, A])(f: F ~> G): EitherK[J, G, A] = hfa.mapK(f)
      |  }
      |
      |  val eitherK: EitherK[Option, List, Int] = EitherK.rightc(List(1, 2, 3))
      |  eitherK.hmap(λ[List ~> Option](_.headOption))
      |}
      |""".stripMargin
  )
}
