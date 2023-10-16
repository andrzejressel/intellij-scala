package org.jetbrains.plugins.scala.lang.dfa.controlFlow.transformations

import com.intellij.codeInspection.dataFlow.lang.ir.SimpleAssignmentInstruction
import org.jetbrains.plugins.scala.extensions.ObjectExt
import org.jetbrains.plugins.scala.lang.dfa.analysis.framework.ScalaStatementAnchor
import org.jetbrains.plugins.scala.lang.dfa.controlFlow.{ScalaDfaControlFlowBuilder, ScalaDfaVariableDescriptor, TransformationFailedException}
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScBindingPattern
import org.jetbrains.plugins.scala.lang.psi.api.expr.{MethodInvocation, ScBlockStatement, ScExpression, ScNewTemplateDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScDefinitionWithAssignment, ScFunctionDefinition, ScPatternDefinition, ScValueOrVariableDefinition, ScVariableDefinition}
import org.jetbrains.plugins.scala.lang.psi.types.ScType

private trait DefinitionTransformer extends Transformer { this: ScalaPsiElementTransformer =>
  def transformDefinition(element: ScDefinitionWithAssignment): Unit = element match {
    case patternDefinition: ScPatternDefinition => transformPatternDefinition(patternDefinition)
    case variableDefinition: ScVariableDefinition => transformVariableDefinition(variableDefinition)
    case _: ScFunctionDefinition => builder.pushUnknownValue()
    case otherStatementDefinition: ScBlockStatement => builder.pushUnknownCall(otherStatementDefinition, 0)
    case _ => throw TransformationFailedException(element, "Unsupported definition.")
  }

  private def transformPatternDefinition(definition: ScPatternDefinition): Unit = {
    transformDefinitionIfSimple(definition, definition.isStable)
  }

  private def transformVariableDefinition(definition: ScVariableDefinition): Unit = {
    transformDefinitionIfSimple(definition, isStable = false)
  }

  private def transformDefinitionIfSimple(definition: ScValueOrVariableDefinition, isStable: Boolean): Unit = {
    if (!definition.isSimple) {
      builder.pushUnknownCall(definition, 0)
    } else {
      val binding = definition.bindings.head
      val descriptor = ScalaDfaVariableDescriptor(binding, None, isStable && binding.isStable)
      val definedType = definition.`type`().getOrAny

      if (definition.expr.exists(canBeClassInstantiationExpression)) {
        assignVariableValueWithInstanceQualifier(descriptor, definition.expr, binding, definedType)
      } else {
        assignVariableValue(descriptor, definition.expr, definedType)
      }

      builder.pushUnknownValue()
    }
  }

  private def canBeClassInstantiationExpression(expression: ScExpression): Boolean = {
    expression.is[ScNewTemplateDefinition, MethodInvocation]
  }

  private def assignVariableValueWithInstanceQualifier(descriptor: ScalaDfaVariableDescriptor,
                                                       instantiationExpression: Option[ScExpression],
                                                       instanceQualifier: ScBindingPattern, definedType: ScType): Unit = {
    val dfaVariable = builder.createVariable(descriptor)
    val anchor = instantiationExpression.map(ScalaStatementAnchor(_)).orNull
    val qualifierVariable = ScalaDfaVariableDescriptor(instanceQualifier, None, instanceQualifier.isStable)

    instantiationExpression match {
      case Some(expression) => transformInvocation(expression, Some(qualifierVariable))
        addImplicitConversion(Some(expression), Some(definedType))
      case _ => builder.pushUnknownValue()
    }

    builder.addInstruction(new SimpleAssignmentInstruction(anchor, dfaVariable))
  }
}
