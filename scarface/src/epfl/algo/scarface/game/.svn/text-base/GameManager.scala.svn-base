package epfl.algo.scarface.game

import epfl.algo.scarface.util.Logging

/**
 * The <code>GameManager</code> manages the games being played by the Scarface player.
 */
object GameManager extends Logging {

  import scala.collection.mutable.{Map, HashMap}
  
  import epfl.algo.scarface.network.AbstractRequestHandler
  import epfl.algo.scarface.player.{AbstractPlayer, AbstractPlayerFactory}
  
  import epfl.algo.scarface.gdl.{Parser}
  import epfl.algo.scarface.gdl.Gdl._
  
  private var playerFactory_ : AbstractPlayerFactory = null
  
  private val games_ : Map[String, AbstractPlayer] = new HashMap[String, AbstractPlayer]
  
  /**
   * Asks the <code>GameManager</code> to process the new request.
   */
  def requestNew(_handler : AbstractRequestHandler) = {
    try {
      _handler.start
    } catch {
      case e => error("Network failure", e)
    }
  }
  
  /**
   * Asks the <code>GameManager</code> to create a new game with the given parameters.
   * 
   * @return the <code>AbstractPlayer</code> to play the game
   */
  def gameNew(_gameId : String, _role : GdlAtom, _gameDescription : GdlList, _startClock : Int, _playClock : Int) : AbstractPlayer = synchronized {
	info("Creating new game [" + _gameId + "]...")
	
	if(playerFactory_ == null) {
      error("Player factory is set to null!")
      return null
    }
    
    if(games_ contains _gameId) {
      error("Game [" + _gameId + "] already exist!")
      return null
    }
    
    // Create and add the player to the list of games
    val player : AbstractPlayer = playerFactory_ playerNew (_gameId, _role, _gameDescription, _startClock, _playClock)
    games_ += ((_gameId, player))
    
    info("New game created with game ID [" + _gameId + "]")
    player
  }
  
  //TODO: stanford signature is somewhat different
  /**
   * Asks the <code>GameManager</code> to terminate the game associated with the provided game identifier.
   */
  def gameEnd(_gameId : String) : Unit = {
	info("Ending game: [" + _gameId + "]")
	games_ get _gameId match {
	  case Some(player) => 	player.stop
	  						games_ - _gameId
	  						info("Ended game [" + _gameId + "]")
	  case None =>	warn("Attempted to end game [" + _gameId + "] which does not exist!")
	}
  }
  
  /**
   * @return the <code>AbstractPlayerFactory</code> used by the <code>GameManager</code>
   */
  def playerFactory : AbstractPlayerFactory = playerFactory_
  
  /**
   * Sets the internal <code>AbstractPlayerFactory</code> to the provided object.
   */
  def playerFactoryIs(_playerFactory : AbstractPlayerFactory) = 
    playerFactory_ = _playerFactory
  
  /**
   * @return the <code>AbstractPlayer</code> associated to the given game identifier if it exists.
   */
  def player(_gameId : String) : Option[AbstractPlayer] = games_ get (_gameId)
  
}
