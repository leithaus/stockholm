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
// uncomment for rlambda evaluation
// import Eval._
// import Compile._

import net.liftweb.amqp._

import scala.collection.immutable.HashMap

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import java.io.StringReader

class REPL {
  // messaging
  var _jsonQueueCnxn : Option[BasicJSONAMQPSender] = None
  def jsonQueueCnxn() : BasicJSONAMQPSender = {
    _jsonQueueCnxn match {
      case Some( jqc ) => jqc
      case None => {
	val jqc = new BasicJSONAMQPSender()
	_jsonQueueCnxn = Some( jqc )
	jqc
      }
    }
  }
  def send( contents : java.lang.Object ) : Unit = {
    jsonQueueCnxn.send( contents )
  }
  // parsing
  def lexer (str : String) = new Yylex( new StringReader( str ) )
  def parser (str : String) = new parser( lexer( str ) )
  def clientRequestParseTree (str : String) = (parser( str )).pExpression()
  def read (str : String) = clientRequestParseTree(str)  

// Uncomment for rlambda evaluation or write and evaluator for your DSL
//   // compilation  
//   def compile( str : String ) =
//     theAssociativeReductionist.compile( str )
  
//   // evaluation  
//   object theAssociativeReductionist
//     extends Compiler
//     with Reduction
//     with Nominals {
//       type Nominal = Either[StringLiteral,Transcription]
//       type Term = Expression
//       type Environment = AssociativeEnvironment
      
//       case class AssociativeEnvironment( map : scala.collection.immutable.Map[ Mention, Value ] ) {
// 	def apply( mention : Mention ) = {
// 	  map.get( mention ) match {
// 	    case Some( v )  => v
// 	    case None => throw new Exception( "not found" )
// 	  }
// 	}
// 	def extend(
// 	  fmls : List[Mention],
// 	  actls : List[Value]
// 	) = {
// 	  val pairs : List[(Mention,Value)] = fmls.zip( actls );
// 	  new AssociativeEnvironment( map ++ pairs )
// 	}
//       }
      
//       def internist() = this
      
//       override def intern( varExpr : Absyn.VariableExpr ) : Nominal = {
// 	varExpr match {
// 	  case atomLiteral : Absyn.AtomLiteral => {
// 	    Left( new StringLiteral( atomLiteral.ident_ ) )
// 	  }
// 	  case transcription : Absyn.Transcription => {
// 	    Right( new Transcription( compile( transcription.expression_ ) ) )
// 	  }
// 	}
//       }
//     }
  
//   def reduce( expr : theAssociativeReductionist.Expression ) : theAssociativeReductionist.Value = {
//     theAssociativeReductionist.reduce( 
//       theAssociativeReductionist.initialApplicator,
//       new theAssociativeReductionist.AssociativeEnvironment(
// 	new
// 	scala.collection.immutable.HashMap[theAssociativeReductionist.Mention,
// 					   theAssociativeReductionist.Value]()
//       )
//       )( expr )
//   }

//   def reduce( str : String ) : theAssociativeReductionist.Value = {
//     reduce( theAssociativeReductionist.compile( str ) )
//   }

  // printing
  def showClientMessageRequest (str : String) = {
    val parseTree = clientRequestParseTree(str);
    send( parseTree )
    new XStream( new JettisonMappedXmlDriver() ).toXML( parseTree )
  }
  def showClientRequestParseTree (str : String) =
    PrettyPrinter.show(clientRequestParseTree(str))  

// uncomment for rlambda evaluation
//   def showClientRequestEvaluation (str : String) =
//     reduce( str ).toString  
}
