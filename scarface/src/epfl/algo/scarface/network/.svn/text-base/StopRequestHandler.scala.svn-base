package epfl.algo.scarface.network

import java.net.Socket

import epfl.algo.scarface.gdl.Gdl._
import epfl.algo.scarface.util.Logging

/**
 * A <code>StopRequestHandler</code> is a handler for request type STOP.
 *
 * @specfield socket_ the <code>Socket</code> to which the request handler is connected
 * @specfield header_ the <code>HTTPHeader</code> of the request received
 * @specfield list_ the <code>GdlList</code> containing game information
 * @specfield gameId_ the identifier of the game being played
 */
class StopRequestHandler (private val socket_ : Socket, private val header_ : HTTPHeader, private val list_ : GdlList, gameId_ : String) extends AbstractRequestHandler(socket_, header_, gameId_) with Logging {

  import epfl.algo.scarface.game.GameManager
  
  // Specified by AbstractRequestHandler superclass
  def execute : Unit = {
	info("[Game " + gameId_ + "] Shutting down player!")
    GameManager gameEnd gameId_
  }
  
}
