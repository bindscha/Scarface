package epfl.algo.scarface.network

import java.net.Socket

import epfl.algo.scarface.gdl.Gdl._
import epfl.algo.scarface.util.Logging

/**
 * A <code>StartRequestHandler</code> is a handler for request type START.
 * 
 * @specfield socket_ the <code>Socket</code> to which the request handler is connected
 * @specfield header_ the <code>HTTPHeader</code> of the request received
 * @specfield list_ the <code>GdlList</code> containing game information
 * @specfield gameId_ the identifier of the game being played
 */
class StartRequestHandler (private val socket_ : Socket, private val header_ : HTTPHeader, private val list_ : GdlList, gameId_ : String) extends AbstractRequestHandler(socket_, header_, gameId_) with Logging {

  import epfl.algo.scarface.game.GameManager
  import epfl.algo.scarface.player.AbstractPlayer
  
  import epfl.algo.scarface.util.HTTPTools._
  
  val list = list_.l_
  
  if(list.size != 6)
    throw new IllegalArgumentException("START request is invalid (should contain 6 arguments, but contains " + list.size + ")")
  
  // Specified by AbstractRequestHandler superclass
  def execute : Unit = {
	info("[Game " + gameId_ + "] Preparing for new game!")
	
    val (role : GdlAtom, gameDescription : GdlList, startClock : Int, playClock : Int) = (list(2), list(3), (list(4)).toString.toInt, (list(5)).toString.toInt)
    
    val player : AbstractPlayer = GameManager.gameNew(gameId_, role, gameDescription, startClock, playClock)
    
    player.start
    
    Thread.sleep((player.startClock - 2) * 1000)
    
    reply(READY)
    
    info("[Game " + gameId_ + "] Ready to play!")
  }
  
}
