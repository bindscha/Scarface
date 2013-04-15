package epfl.algo.scarface.network

import epfl.algo.scarface.util.Logging
class ConnectionManager(port_ : Int, factory_ : RequestHandlerFactory) extends Thread with Logging {
 
  import java.io.IOException
  import java.net.{Socket, ServerSocket}
  
  import epfl.algo.scarface.game.GameManager
  
  val serverSocket = new ServerSocket(port_)
  var running = true
  
  override def run : Unit = {
    
    var socket : Socket = null
    
    def grabSocket : Boolean = 
	    try {
	      socket = serverSocket.accept
	      socket != null
	    } catch {
	    	case e => false
	    }
    
    try {
	    while (grabSocket) {
	      val hostname = socket.getInetAddress.getHostName
	      info("Incoming connection from " + hostname)
	      
	      try {
	        val handler : AbstractRequestHandler = factory_.requestHandlerNew(this, socket)
	        GameManager.requestNew(handler)
	      } catch {
	        case e : Exception => error("Error occurred while handling request from " + hostname, e)
	      }
	    }
    } catch {
      case e : IOException if running => error("Network failure!", e)
    }
      
  }
  
  def shutdown = {
    if(running) {
      try {
        serverSocket.close
        join
      }
    }
  }
  
}
