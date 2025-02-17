package org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef

import com.intellij.lang.ASTNode
import com.intellij.openapi.project.DumbService
import com.intellij.openapi.util.TextRange
import com.intellij.psi.PsiModifier._
import com.intellij.psi._
import com.intellij.psi.scope.PsiScopeProcessor
import com.intellij.psi.tree.IElementType
import com.intellij.psi.util.PsiUtil
import org.jetbrains.plugins.scala.caches.{BlockModificationTracker, cached}
import org.jetbrains.plugins.scala.icons.Icons
import org.jetbrains.plugins.scala.lang.lexer.{ScalaTokenType, ScalaTokenTypes}
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil.getCompanionModule
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef._
import org.jetbrains.plugins.scala.lang.psi.impl.base.ScNamedBeginImpl
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.ScObjectImpl.LegacyPackageObjectNameInBackticks
import org.jetbrains.plugins.scala.lang.psi.impl.{ScPackageImpl, ScalaPsiManager}
import org.jetbrains.plugins.scala.lang.psi.light.{EmptyPrivateConstructor, PsiClassWrapper, ScLightField}
import org.jetbrains.plugins.scala.lang.psi.stubs.ScTemplateDefinitionStub
import org.jetbrains.plugins.scala.lang.psi.stubs.elements.ScTemplateDefinitionElementType
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.ScDesignatorType
import org.jetbrains.plugins.scala.lang.resolve.ScalaResolveState.ResolveStateExt
import org.jetbrains.plugins.scala.util.ScalaBytecodeConstants.ObjectSingletonInstanceName

class ScObjectImpl(
  stub:      ScTemplateDefinitionStub[ScObject],
  nodeType:  ScTemplateDefinitionElementType[ScObject],
  node:      ASTNode,
  debugName: String
) extends ScTypeDefinitionImpl(stub, nodeType, node, debugName)
    with ScObject
  with ScNamedBeginImpl {

  override protected def targetTokenType: ScalaTokenType = ScalaTokenType.ObjectKeyword

  override def additionalClassJavaName: Option[String] =
    if (baseCompanion.isEmpty) Option(getName).map(_.stripSuffix("$")) else None

  override def getNavigationElement: PsiElement = {
    if (isSyntheticObject) {
      getCompanionModule(this) match {
        case Some(clazz) => return clazz.getNavigationElement
        case _ =>
      }
    }
    super.getNavigationElement
  }

  override def getContainingFile: PsiFile = {
    if (isSyntheticObject) syntheticNavigationElement.getContainingFile
    else super.getContainingFile
  }

  override def getName: String =
    (if (isPackageObject) "package" else super.getName) + "$"

  //noinspection TypeAnnotation
  override protected def baseIcon =
    if (isPackageObject) Icons.PACKAGE_OBJECT else Icons.OBJECT

  // TODO Should be unified, see ScModifierListOwner
  override def hasModifierProperty(name: String): Boolean = name match {
    case FINAL => true
    case _ => super[ScTypeDefinitionImpl].hasModifierProperty(name)
  }

  override final def isEffectivelyFinal: Boolean = true

  override def isObject : Boolean = true

  override def isPackageObject: Boolean = byStubOrPsi(_.isPackageObject) {
    hasPackageKeyword || isPackageObjectLegacy
  }

  override def isPackageObjectNonLegacy: Boolean =
    isPackageObject && name != LegacyPackageObjectNameInBackticks

  override def isPackageObjectLegacy: Boolean =
    name == LegacyPackageObjectNameInBackticks

  override def hasPackageKeyword: Boolean = findChildByType[PsiElement](ScalaTokenTypes.kPACKAGE) != null

  override def isCase: Boolean = hasModifierProperty("case")

  override def syntheticMembers: Seq[ScMember] = ScalaPsiUtil.getCompanionModule(this) match {
    case Some(e: ScEnum) => e.cases ++ super.syntheticMembers
    case _ => super.syntheticMembers
  }

  override def processDeclarationsForTemplateBody(
    processor:  PsiScopeProcessor,
    state:      ResolveState,
    lastParent: PsiElement,
    place:      PsiElement
  ): Boolean =
    if (DumbService.getInstance(getProject).isDumb) true
    else if (!super.processDeclarationsForTemplateBody(processor, state, lastParent, place)) false
    else if (isPackageObjectNonLegacy) {
      JavaPsiFacade.getInstance(getProject)
        // do not wrap into ScPackage to avoid SOE
        .findPackage(qualifiedName) match {
        case null => true
        case pack =>
          val newState = state.withFromType(None)

          ScPackageImpl.packageProcessDeclarations(pack)(processor, newState, lastParent, place)(ScalaPsiManager.instance)
      }
    } else true

  override def processDeclarations(
    processor:  PsiScopeProcessor,
    state:      ResolveState,
    lastParent: PsiElement,
    place:      PsiElement
  ): Boolean = processDeclarationsImpl(processor, state, lastParent, place)

  override def fakeCompanionClass: Option[PsiClass] = _fakeCompanionClass()

  private val _fakeCompanionClass = cached("fakeCompanionClass", BlockModificationTracker(this), () => {
    getCompanionModule(this) match {
      case Some(_) => None
      case None =>
        val qualName = Option(getQualifiedName).map(_.stripSuffix("$"))
        val name = Option(getName).map(_.stripSuffix("$"))
        name.map(new PsiClassWrapper(this, qualName.orNull, _))
    }
  })

  override def fakeCompanionClassOrCompanionClass: PsiClass = fakeCompanionClass match {
    case Some(clazz) => clazz
    case _ => getCompanionModule(this).get
  }

  private val getModuleField: () => Option[PsiField] = cached("getModuleField", BlockModificationTracker(this), () => {
    def hasJavaKeywords(qName: String) =
      qName.split('.').exists(PsiUtil.isKeyword(_, PsiUtil.getLanguageLevel(this.getProject)))

    if (Option(getQualifiedName).forall(hasJavaKeywords))
      None
    else
      Some(ScLightField(ObjectSingletonInstanceName, ScDesignatorType(this), this, PUBLIC, FINAL, STATIC))
  })

  override def psiFields: Array[PsiField] = {
    getModuleField().toArray
  }

  override def findFieldByName(name: String, checkBases: Boolean): PsiField = {
    name match {
      case ObjectSingletonInstanceName =>
        getModuleField().orNull
      case _ => null
    }
  }

  override def psiInnerClasses: Array[PsiClass] = Array.empty

  override def getConstructors: Array[PsiMethod] = _getConstructors()

  private val _getConstructors: () => Array[PsiMethod] = cached("getConstructors", BlockModificationTracker(this), () => {
    Array(new EmptyPrivateConstructor(this))
  })

  override def isPhysical: Boolean = {
    if (isSyntheticObject) false
    else super.isPhysical
  }

  override def getTextRange: TextRange = {
    if (isSyntheticObject) getNavigationElement.getTextRange
    else super.getTextRange
  }

  override def getInterfaces: Array[PsiClass] = {
    getSupers.filter(_.isInterface)
  }

  override protected def keywordTokenType: IElementType = ScalaTokenType.ObjectKeyword

  override def namedTag: Option[ScNamedElement] = Some(this)

  override protected def endParent: Option[PsiElement] = extendsBlock.templateBody
}

object ScObjectImpl {
  /**
   * Long time ago, prior to Scala 2.8, package objects were defined using this syntax: {{{
   *   package org.example
   *
   *   object `package` {
   *   }
   * }}}
   *
   * Even though it's not a recommended way to write a package object in the latest Scala versions,
   * it's still supported and we should at least not show red code for it
   *
   * See also [[org.jetbrains.plugins.scala.util.ScalaBytecodeConstants.PackageObjectClassName]]<br>
   * See also [[org.jetbrains.plugins.scala.util.ScalaBytecodeConstants.PackageObjectClassPackageSuffix]]<br>
   */
  val LegacyPackageObjectNameInBackticks: String = "`package`"

  /** See docs of [[LegacyPackageObjectNameInBackticks]] */
  val LegacyPackageObjectPackageSuffix: String = ".package"

  def stripLegacyPackageObjectSuffixWithDot(packageName: String): String =
    packageName
      .stripSuffix(LegacyPackageObjectNameInBackticks)
      .stripSuffix(".")
}
