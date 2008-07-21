package org.jetbrains.plugins.scala.lang.parser.parsing.top

import com.intellij.lang.PsiBuilder
import com.intellij.psi.tree.IElementType
import com.intellij.psi.tree.IChameleonElementType
import com.intellij.psi.tree.TokenSet

import org.jetbrains.plugins.scala.util.DebugPrint
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.parser.ScalaElementTypes
import org.jetbrains.plugins.scala.lang.lexer.ScalaElementType
import org.jetbrains.plugins.scala.lang.parser.parsing.types.Type
import org.jetbrains.plugins.scala.lang.parser.util.ParserUtils
import org.jetbrains.plugins.scala.lang.parser.parsing.types.SimpleType
import org.jetbrains.plugins.scala.lang.parser.bnf.BNF
import org.jetbrains.plugins.scala.lang.parser.parsing.top.template.TemplateBody
import org.jetbrains.plugins.scala.lang.parser.parsing.top.template.ClassParents




import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.lang.parser.parsing.base.AccessModifier
import org.jetbrains.plugins.scala.lang.parser.parsing.nl.LineTerminator

/** 
* @author Alexander Podkhalyuzin
* Date: 06.03.2008
* Time: 9:31:16
*/

/*
 * ClassTemplate ::= [EarlyDefs] ClassParents [TemplateBody]
 *                 | TemplateBody (for 'new' statement)
 */

object ClassTemplate {
  def parse(builder: PsiBuilder): Boolean = parse(builder, false)
  def parse(builder: PsiBuilder, nonEmpty: Boolean) : Boolean = {
     val extendsMarker = builder.mark
     var empty = true
     builder.getTokenType match {
      //hardly case, becase it's same token for ClassParents and TemplateBody
      case ScalaTokenTypes.tLBRACE => {
        empty = false
        //try to parse early definition if we can't => it's template body
        if (EarlyDef parse builder) {
          ClassParents parse builder
          //parse template body
          builder.getTokenType match {
            case ScalaTokenTypes.tLBRACE => {
              TemplateBody parse builder
              extendsMarker.done(ScalaElementTypes.EXTENDS_BLOCK)
              return !nonEmpty || !empty
            }
            case ScalaTokenTypes.tLINE_TERMINATOR => {
              if (!LineTerminator(builder.getTokenText)) {
                extendsMarker.done(ScalaElementTypes.EXTENDS_BLOCK)
                return !nonEmpty || !empty
              }
              else {
                val backup = builder.mark
                builder.advanceLexer //Ate nl
                builder.getTokenType match {
                  case ScalaTokenTypes.tLBRACE => {
                    TemplateBody parse builder
                    extendsMarker.done(ScalaElementTypes.EXTENDS_BLOCK)
                    return !nonEmpty || !empty
                  }
                  case _ => {
                    backup.rollbackTo
                    extendsMarker.done(ScalaElementTypes.EXTENDS_BLOCK)
                    return !nonEmpty || !empty
                  }
                }
              }
            }
            case _ => {
              extendsMarker.done(ScalaElementTypes.EXTENDS_BLOCK)
              return !nonEmpty || !empty
            }
          }
        }
        else {
          //parse template body
          TemplateBody parse builder
          extendsMarker.done(ScalaElementTypes.EXTENDS_BLOCK)
          return !nonEmpty || !empty
        }
      }
      //if we find nl => it could be TemplateBody only, but we can't find nl after extends keyword
      //In this case of course it's ClassParents
      case _ => {
        if (ClassParents parse builder) empty = false
        //parse template body
        builder.getTokenType match {
          case ScalaTokenTypes.tLBRACE => {
            TemplateBody parse builder
            extendsMarker.done(ScalaElementTypes.EXTENDS_BLOCK)
            return !nonEmpty || !empty
          }
          case ScalaTokenTypes.tLINE_TERMINATOR => {
              if (!LineTerminator(builder.getTokenText)) {
                extendsMarker.done(ScalaElementTypes.EXTENDS_BLOCK)
                return !nonEmpty || !empty
              }
              else {
                val backup = builder.mark
                builder.advanceLexer //Ate nl
                builder.getTokenType match {
                  case ScalaTokenTypes.tLBRACE => {
                    TemplateBody parse builder
                    extendsMarker.done(ScalaElementTypes.EXTENDS_BLOCK)
                    return !nonEmpty || !empty
                  }
                  case _ => {
                    backup.rollbackTo
                    extendsMarker.done(ScalaElementTypes.EXTENDS_BLOCK)
                    return  !nonEmpty || !empty
                  }
                }
              }
            }
          case _ => {
            extendsMarker.done(ScalaElementTypes.EXTENDS_BLOCK)
            return !nonEmpty || !empty
          }
        }
      }
    }
  }
}