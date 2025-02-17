package org.jetbrains.sbt
package lang.completion

import org.jetbrains.plugins.scala.util.RevertableChange

class SbtCompletionScalacOptionsTest extends SbtFileTestDataCompletionTestBase with MockSbt_1_0 {
  override def folderPath: String = super.folderPath + "scalacOptions/"

  def testCompleteSeqRef(): Unit = doTest()

  def testCompleteSeqRefWithDash(): Unit = doTest()

  def testCompleteSeqRefWithScope(): Unit = doTest()

  def testCompleteSeqRefWithDashAndScope(): Unit = doTest()

  def testCompleteSeqStringLiteral(): Unit = doTest()

  def testCompleteSeqStringLiteralWithDash(): Unit = doTest()

  def testCompleteSeqStringLiteralWithDashAndScope(): Unit = doTest()

  def testCompleteSeqStringLiteralWithScope(): Unit = doTest()

  def testCompleteSingleRef(): Unit = doTest()

  def testCompleteSingleRefWithDash(): Unit = doTest()

  def testCompleteSingleRefWithScope(): Unit = doTest()

  def testCompleteSingleRefWithDashAndScope(): Unit = doTest()

  def testCompleteSingleStringLiteral(): Unit = doTest()

  def testCompleteSingleStringLiteralWithDash(): Unit = doTest()

  def testCompleteSingleStringLiteralWithDashAndScope(): Unit = doTest()

  def testCompleteSingleStringLiteralWithScope(): Unit = doTest()

  def testCompleteLowerCase(): Unit = doTest()

  def testCompleteUpperCase(): Unit = doTest()

  def testCompleteComplexExprRef(): Unit = doTest()

  // TODO(SCL-22236): ERROR: Tree access disabled
  def testCompleteComplexExprStringLiteral(): Unit = RevertableChange.withModifiedRegistryValue("ast.loading.filter", newValue = false) {
    doTest()
  }

  def testCompleteObjectMemberAfterScalacOptions(): Unit = doTest()

  def testCompleteObjectMemberAfterScalacOptionsInInterpolatedString(): Unit = doTest()

  def testCompleteInInterpolatedString(): Unit = doTest()
}
