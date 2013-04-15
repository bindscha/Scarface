package epfl.algo.scarface.network

import java.net.Socket

import epfl.algo.scarface.util.Logging

/**
 * <code>HTTPHeader</code> represents an HTTP Header.
 *
 * @specfield sender_ the name of the HTTP message sender
 * @specfield receiver_ the name of the receiver of the HTTP message
 * @specfield contentLength_ the length of the HTTP message
 */
class HTTPHeader(var sender_ : String, var receiver_ : String, var contentLength_ : Int)

/**
 * <code>AbstractRequestHandler</code> represents a basic request handler for GGP.
 * The GGP specification provides 4 request types: PLAY, START, STOP and KILL which are all subclasses of this class.
 *
 * @specfield socket_ the <code>Socket</code> to which the request handler is connected
 * @specfield header_ the <code>HTTPHeader</code> of the request received
 * @specfield gameId_ the identifier of the game being played
 */
abstract class AbstractRequestHandler (private val socket_ : Socket, private val header_ : HTTPHeader, private val gameId_ : String) extends Thread with Logging {
  
  import java.io.PrintStream
  
  import epfl.algo.scarface.util.HTTPTools._
  
  private val writer_ = new PrintStream(socket_.getOutputStream())
  
  private var connectionManager_ : ConnectionManager = null
  
  /**
   * @return the <code>ConnectionManager</code> used.
   */
  def connectionManager : ConnectionManager = connectionManager_
  
  /**
   * Sets the <code>ConnectionManager</code> of this request to the provided object.
   */
  def connectionManagerIs(connectionManager : ConnectionManager) : Unit =
    connectionManager_ = connectionManager
  
  /**
   * Perform the request.
   */
  protected def execute : Unit
  
  /**
   * Terminates the request handler.
   */
  def finish : Unit = {
    socket_.close
    debug("Request handler terminated")
  }
  
  /**
   * Starts execution of the request handler.
   */
  override def run : Unit = 
    try {
      debug("Executing request handler")
      execute
      debug("Done executing request handler")
      System.gc //TODO: should it be done?
      finish
    } catch {
      case e => error("Exception thrown while executing request handler", e)
    }
  
  /**
   * Sends a reply to the server.
   */
  def reply(_reply : String) : Unit = {
    writer_ print REPLY_HEADER
    writer_ print _reply.length + SEPARATOR + SEPARATOR
    writer_ print _reply
    writer_.flush
    debug("Sent message to server: " + _reply)
  }

}
