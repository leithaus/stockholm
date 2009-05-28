// -*- mode: Scala;-*- 
// Filename:    JSONAMQPDispatcher.scala 
// Authors:     lgm                                                    
// Creation:    Tue May 26 06:40:08 2009 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package net.liftweb.amqp

import _root_.com.rabbitmq.client._
import _root_.scala.actors.Actor
import _root_.java.io.ObjectInputStream
import _root_.java.io.ByteArrayInputStream
import _root_.java.util.Timer
import _root_.java.util.TimerTask

import _root_.com.thoughtworks.xstream.XStream
import _root_.com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import javax.persistence.EntityManager
import javax.persistence.EntityManagerFactory
import javax.persistence.Persistence

import org.apache.log4j.Logger
import org.apache.log4j.PropertyConfigurator
import java.util.Properties
import java.io.FileInputStream
import java.io.IOException

class JSONSerializedAMQPDispatcher[T](
  factory: ConnectionFactory,
  host: String,
  port: Int
) extends AMQPDispatcher[T](factory, host, port) {  
  override def configure(channel: Channel) {
    // Get the ticket.
    val ticket = channel.accessRequest("/data")
    // Set up the exchange and queue
    channel.exchangeDeclare(ticket, "mult", "direct")
    channel.queueDeclare(ticket, "mult_queue")
    channel.queueBind(ticket, "mult_queue", "mult", "routeroute")
    // Use the short version of the basicConsume method for convenience.
    channel.basicConsume(ticket, "mult_queue", false, new SerializedConsumer(channel, this))
  }  
}

trait JSONHandler {
  def handle( contents: String ) : Unit = {
    new XStream( new JettisonMappedXmlDriver() ).fromXML( contents )
  }
}

trait JSONToSQLHandler {
  var _emf : Option[EntityManagerFactory] = None
  def entityMgrFactory( db : String ) : EntityManagerFactory = {
    _emf match {
      case Some( emf ) => emf
      case None => {
	val emf = Persistence.createEntityManagerFactory( db )
	_emf = Some( emf )
	emf
      }
    }
  }
  var _em : Option[EntityManager] = None
  def entityManager( db : String ) : EntityManager = {
    _em match {
      case Some( em ) => em
      case None => {
	val em = entityMgrFactory( db ).createEntityManager()
	_em = Some( em )
	em
      }
    }
  }
  def handle( db : String )( contents: String ) : Unit = {
    var obj : java.lang.Object = null;
    try {
      obj = 
	new XStream(
	  new JettisonMappedXmlDriver()
	).fromXML( contents );
      
      try {
	entityManager( db ).getTransaction().begin();
	entityManager( db ).persist( obj );
	entityManager( db ).getTransaction().commit();
      }
      catch {
	case e => {
	  println( "persistence error" )
	  e.printStackTrace
	}
      }
    }
    catch {
      case e => {
	  println( "marshaling error" )
	  e.printStackTrace
	}
    }
  }
}

class JSONAMQPListener {
  val LOG_PROPERTIES_FILE : String =
    "src/main/resources/Log4J.properties";  

  val params = new ConnectionParameters
  params.setUsername("guest")
  params.setPassword("guest")
  params.setVirtualHost("/")
  params.setRequestedHeartbeat(0)

  val factory = new ConnectionFactory(params)
  
  val amqp =
    new JSONSerializedAMQPDispatcher[String](
      factory,
      "localhost",
      5672
    )

  def configureLogging() {
    val logProperties : Properties = new Properties();
    val log : Logger =
      Logger.getLogger(classOf[JSONAMQPListener]);
    try {      
      logProperties.load(new FileInputStream(LOG_PROPERTIES_FILE));
      PropertyConfigurator.configure(logProperties);
      log.info("Logging initialized.");
    }
    catch {
      case e => e.printStackTrace
    }
  }
  
  def testHandle = {
    val jal = new net.liftweb.amqp.JSONAMQPListener
    jal.amqp ! net.liftweb.amqp.AMQPReconnect( 4 )
    jal.jsonListener.handle( "stockholm" )(
      new com.thoughtworks.xstream.XStream(
	new com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver
      ).toXML(
	(new com.biosimilarity.reflection.model.REPL).read( "lambda x.x" )
      )
    )
  }

  amqp.start

  // JSON Listener
  class JSONListener( logging : Boolean )
  extends Actor
  with JSONToSQLHandler {
    def act = {
      react {
	case msg@AMQPMessage( contents : String ) => {
	  if ( logging ) {
	    println("received: " + msg)
	  };
	  handle( "stockholm" )( contents );
	  act
	}
      }
    }
  }
  val jsonListener = new JSONListener( true )
  jsonListener.start
  amqp ! AMQPAddListener(jsonListener)
}