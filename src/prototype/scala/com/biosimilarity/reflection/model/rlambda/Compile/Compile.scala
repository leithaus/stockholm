// -*- mode: Scala;-*- 
// Filename:    Compile.scala 
// Authors:     lgm                                                    
// Creation:    Mon May  4 16:22:54 2009 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.reflection.model.rlambda.Compile

import com.biosimilarity.reflection.model.rlambda._
import Absyn._
import Eval._

import java.io.StringReader

trait Compiler extends Expressions with Nominals {
  type Internist = {def intern( varExpr : Absyn.VariableExpr ) : Nominal}
  def internist() : Internist

  def intern( varExpr : Absyn.VariableExpr ) : Nominal = { internist().intern( varExpr ) }
  def compileExpr( numericExpr : Absyn.Numeric ) : Expression = {
    new IntegerExpression( numericExpr.integer_.asInstanceOf[Int] )
  }
  def compileExpr( mentionExpr : Absyn.Mention ) : Expression = {
     new Mention( intern( mentionExpr.variableexpr_ ) )
  }
  def compileExpr( abstractionExpr : Absyn.Abstraction ) : Expression = {
    val fmls : List[Nominal] =
      scala.collection.jcl.Conversions.convertList(
	abstractionExpr.listvariableexpr_
      ).map({ ( vExpr : Absyn.VariableExpr ) => intern( vExpr )  }).toList
    new Abstraction( fmls, compile( abstractionExpr.expression_ ) )	    
  }
  def compileExpr( applicationExpr : Absyn.Application ) : Expression = {
    new Application(
      compile( applicationExpr.expression_1 ),
      List( compile( applicationExpr.expression_2 ) )
    )
  }

  def compile( expr : Absyn.Expression ) : Expression = {
    expr match {
      case value : Absyn.Value => {
	value.valueexpr_ match {
	  case numericExpr : Absyn.Numeric =>
	    compileExpr( numericExpr )
	}
      }
      case numericExpr : Absyn.Numeric => {
	compileExpr( numericExpr )
      }
      case mentionExpr : Absyn.Mention => {
	compileExpr( mentionExpr )
      }
      case abstractionExpr : Absyn.Abstraction => {
	compileExpr( abstractionExpr )	    
      }
      case applicationExpr : Absyn.Application => {
	compileExpr( applicationExpr )
      }
    }
  }

  def parse( str : String ) : Absyn.Expression = {
    (new parser( new Yylex( new StringReader( str ) ) ) ).pExpression()
  }

  def compile( str : String ) : Expression = {
    try {
      compile( parse( str ) )
    }
    catch {
      case e => { // log error 
	throw e
      }
    }
  }
}
