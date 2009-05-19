// -*- mode: Scala;-*- 
// Filename:    REPL.scala 
// Authors:     lgm                                                    
// Creation:    Thu May  8 10:18:48 2008 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.reflection.model

import com.biosimilarity.reflection.model.rlambda._

import Absyn._
import Eval._
import Compile._

import scala.collection.immutable.HashMap

import java.io.StringReader

class REPL {
  // parsing
  def lexer (str : String) = new Yylex( new StringReader( str ) )
  def parser (str : String) = new parser( lexer( str ) )
  def clientRequestParseTree (str : String) = (parser( str )).pExpression()
  def read (str : String) = clientRequestParseTree(str)  

  // compilation  
  def compile( str : String ) =
    theAssociativeReductionist.compile( str )
  
  // evaluation  
  object theAssociativeReductionist
    extends Compiler
    with Reduction
    with Nominals {
      type Nominal = Either[StringLiteral,Transcription]
      type Term = Expression
      type Environment = AssociativeEnvironment
      
      case class AssociativeEnvironment( map : scala.collection.immutable.Map[ Mention, Value ] ) {
	def apply( mention : Mention ) = {
	  map.get( mention ) match {
	    case Some( v )  => v
	    case None => throw new Exception( "not found" )
	  }
	}
	def extend(
	  fmls : List[Mention],
	  actls : List[Value]
	) = {
	  val pairs : List[(Mention,Value)] = fmls.zip( actls );
	  new AssociativeEnvironment( map ++ pairs )
	}
      }
      
      def internist() = this
      
      override def intern( varExpr : Absyn.VariableExpr ) : Nominal = {
	varExpr match {
	  case atomLiteral : Absyn.AtomLiteral => {
	    Left( new StringLiteral( atomLiteral.ident_ ) )
	  }
	  case transcription : Absyn.Transcription => {
	    Right( new Transcription( compile( transcription.expression_ ) ) )
	  }
	}
      }
    }
  
  def reduce( expr : theAssociativeReductionist.Expression ) : theAssociativeReductionist.Value = {
    theAssociativeReductionist.reduce( 
      theAssociativeReductionist.initialApplicator,
      new theAssociativeReductionist.AssociativeEnvironment(
	new
	scala.collection.immutable.HashMap[theAssociativeReductionist.Mention,
					   theAssociativeReductionist.Value]()
      )
      )( expr )
  }

  def reduce( str : String ) : theAssociativeReductionist.Value = {
    reduce( theAssociativeReductionist.compile( str ) )
  }

  // printing
  def showClientRequestParseTree (str : String) =
    PrettyPrinter.show(clientRequestParseTree(str))  
  def showClientRequestEvaluation (str : String) =
    reduce( str ).toString  
}
