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

class JSONAMQPListener {
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
  amqp.start

  // JSON Listener
  class JSONListener( logging : Boolean )
  extends Actor
  with JSONHandler {
    def act = {
      react {
	case msg@AMQPMessage( contents : String ) => {
	  if ( logging ) {
	    println("received: " + msg)
	  };
	  handle( contents );
	  act
	}
      }
    }
  }
  val jsonListener = new JSONListener( true )
  jsonListener.start
  amqp ! AMQPAddListener(jsonListener)
}
