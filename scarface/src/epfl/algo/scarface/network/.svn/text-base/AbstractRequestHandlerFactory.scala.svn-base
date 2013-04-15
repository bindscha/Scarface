package epfl.algo.scarface.network

import epfl.algo.scarface.util.Logging

/**
 * <code>AbstractRequestHandlerFactory</code> represents a basic request handler factory object for GGP.
 * The GGP specification provides 4 request types: PLAY, START, STOP and KILL which are all created by this class.
 */
abstract class AbstractRequestHandlerFactory extends Logging {

  import java.net.Socket
  
  import epfl.algo.scarface.game.GameManager
  import epfl.algo.scarface.gdl.Parser
  import epfl.algo.scarface.gdl.Gdl.GdlList
  import epfl.algo.scarface.network.AbstractRequestHandler
  import epfl.algo.scarface.util.{LineInputStream, LengthInputStream}
  
  /**
   * Creates and returns a new <code>AbstractRequestHandler</code> by listening to the provided connection.
   */
  def requestHandlerNew(_manager : ConnectionManager, _socket : Socket) : AbstractRequestHandler = synchronized {
    debug("Starting to create a new request handler")
    
    // Receive and parse the HTTP message
    val input = new LineInputStream(_socket.getInputStream())
    
    val header = readHeader(input)
    
    val contentInput = new LengthInputStream(input, header.contentLength_)
    
    val str = new StringBuilder
    
    var stop = false
    while(!stop) {
    	val read = contentInput.read
    	if(read == -1)
    		stop = true
    	else
    		str.append(read.asInstanceOf[Char])
    }
    
    val parser : Parser = new Parser(str.toString)
    
    val content = parser.game
    
    // Call the specific request creator
    val handler = requestHandlerNew(_socket, header, content) 
    
    debug("Request handler created")
    handler
  }
  
  /**
   * @return an <code>HTTPHeader</code> object representing the header of the received message.
   */
  private def readHeader(_input : LineInputStream) : HTTPHeader = {
    debug("Parsing header")
    
    var line : String = null
    
    def grabLine : Boolean = {
      line = _input.readLine
      line != null
    }
    
    val header = new HTTPHeader(null, null, 0)
    
    while(grabLine && line.trim != "") {
      if (line.startsWith("Sender:"))
        header.sender_ = line.substring(8);
      else if ( line.startsWith("Receiver:") )
        header.receiver_ = line.substring(10);
      else if ( line.startsWith("Content-length:") || line.startsWith("Content-Length:") )
        header.contentLength_ = Integer.parseInt(line.substring(16));
    }
    
    debug("Header parsed. ContentLength=" + header.contentLength_)
    
    header
    
  }
  
  /**
   * Creates a new request handler for the given request.
   */
  protected def requestHandlerNew(_socket : Socket, _header : HTTPHeader, _list : GdlList) : AbstractRequestHandler
  
}
