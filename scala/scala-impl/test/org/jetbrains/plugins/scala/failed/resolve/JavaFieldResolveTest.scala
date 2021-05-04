package org.jetbrains.plugins.scala.failed.resolve

/**
  * @author Nikolay.Tropin
  */
class JavaFieldResolveTest extends FailableResolveTest("javaField") {
  def testSCL6925(): Unit = doTest()
  def testSCL12413(): Unit = doTest()
  def testSCL12630(): Unit = doTest()
}
