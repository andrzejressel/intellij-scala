package org.jetbrains.plugins.scala.failed.typeInference

import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestCase

class MixinTypeTest extends ScalaLightCodeInsightFixtureTestCase{

  override protected def shouldPass: Boolean = false

  def testSCL13112(): Unit = {
    val text =
      """
        |trait A
        |trait B
        |trait C[T]
        |
        |object X {
        |  def f(x : C[A with B]) : C[B with A] = x
        |}
      """.stripMargin
    checkTextHasNoErrors(text)
  }
}
