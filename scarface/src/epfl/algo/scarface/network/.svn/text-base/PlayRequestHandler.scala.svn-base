package epfl.algo.scarface.network

import java.net.Socket

import epfl.algo.scarface.gdl.Gdl._
import epfl.algo.scarface.util.Logging

/**
 * A <code>PlayRequestHandler</code> is a handler for request type PLAY.
 * 
 * @specfield socket_ the <code>Socket</code> to which the request handler is connected
 * @specfield header_ the <code>HTTPHeader</code> of the request received
 * @specfield list_ the <code>GdlList</code> containing game information
 * @specfield gameId_ the identifier of the game being played
 */
class PlayRequestHandler(private val socket_ : Socket, private val header_ : HTTPHeader, private val list_ : GdlList, gameId_ : String) extends AbstractRequestHandler(socket_, header_, gameId_) with Logging {

  import epfl.algo.scarface.game.GameManager
  
  import epfl.algo.scarface.util.HTTPTools._
  
  val list = list_.l_
  
  if(list.size != 3)
    throw new IllegalArgumentException("PLAY request is invalid (should contain 3 arguments, but contains " + list.size + ")")
  
  // Specified by AbstractRequestHandler superclass
  def execute : Unit = {
	info("[Game " + gameId_ + "] Preparing to play our next move...")
    GameManager player gameId_ match {
      case Some(player) =>
      	// Update the player to the latest moves
        list(2) match {
        	case lst : GdlList => player update lst
        	case _ => 
        }
        
        Thread.sleep((player.playClock - 2) * 1000)
        
        // Ask the player to provide a move and forward it to the server
        val (move : GdlTerm, expl : String, t : String) = player play
        val response = 	move.toString.toUpperCase //+ 
          				//(if(expl != null) " " + explanation(expl)) +
          				//(if(t != null) " " + taunt(t))
        reply(response)
        info("[Game " + gameId_ + "] Replied to server with move: " + response)
      case None =>
        error("[Game " + gameId_ + "] Game doesn't exist!")
        finish
    }
  }
  
}
