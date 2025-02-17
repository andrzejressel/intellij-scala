package org.jetbrains.plugins.scala.lang.checkers.checkPrivateAccess

import com.intellij.openapi.util.io.FileUtil
import com.intellij.openapi.util.text.StringUtil
import com.intellij.openapi.vfs.{CharsetToolkit, LocalFileSystem}
import com.intellij.psi.PsiMember
import com.intellij.psi.util.PsiTreeUtil
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestCase
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.base.ScReference
import org.jetbrains.plugins.scala.lang.resolve.ResolveUtils
import org.jetbrains.plugins.scala.util.TestUtils
import org.jetbrains.plugins.scala.util.TestUtils.ExpectedResultFromLastComment
import org.junit.Assert._

import java.io.File

abstract class CheckPrivateAccessTestBase extends ScalaLightCodeInsightFixtureTestCase {
  val refMarker = "/*ref*/"

  protected def folderPath = getTestDataPath + "checkers/checkPrivateAccess/"

  override protected def shouldPass: Boolean = true

  protected def doTest(): Unit = {
    val filePath = folderPath + getTestName(false) + ".scala"
    val file = LocalFileSystem.getInstance.findFileByPath(filePath.replace(File.separatorChar, '/'))
    assertNotNull("file " + filePath + " not found", file)

    val fileText = StringUtil.convertLineSeparators(FileUtil.loadFile(new File(file.getCanonicalPath), CharsetToolkit.UTF8))
    configureFromFileText(getTestName(false) + ".scala", fileText)
    val scalaFile = getFile.asInstanceOf[ScalaFile]
    val offset = fileText.indexOf(refMarker) + refMarker.length
    assertNotEquals("Not specified caret marker in test case. Use " + refMarker + " in scala file for this.", offset, refMarker.length - 1)

    val elem = scalaFile.findElementAt(offset).getParent
    if (!elem.isInstanceOf[ScReference])
      fail("Ref marker should point on reference")
    val ref = elem.asInstanceOf[ScReference]
    val resolve: PsiMember = PsiTreeUtil.getParentOfType(ref.resolve(), classOf[PsiMember], false)

    val actual = ResolveUtils.isAccessible(resolve, elem)

    val ExpectedResultFromLastComment(_, expected) = TestUtils.extractExpectedResultFromLastComment(scalaFile)

    if (shouldPass) {
      assertEquals("Wrong reference accessibility: ", expected, actual.toString)
    }
    else {
      if (expected == actual.toString) {
        fail("Test has passed, but was supposed to fail")
      }
    }
  }
}
