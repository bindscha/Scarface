package epfl.algo.scarface.player

import epfl.algo.scarface.gdl.{Parser, GameInfo}
import epfl.algo.scarface.gdl.Gdl._
    
import epfl.algo.scarface.proof.{GenericLogicFactory, AbstractReasoner, State, TuPrologWrapper}

/**
 * A <code>RandomPlayer</code> is the simplest kind of players there is as it plays a random move at each turn.
 * 
 * @specfield gameId_ the identifier of the game being played
 * @specfield parser_ the GDL <code>Parser</code> used
 * @specfield reasoner_ the <code>AbstractReasoner</code> used to reason about games
 * @specfield role_ the <code>GdlAtom</code> representing the role of this player
 * @specfield gameInformation_ the <code>GameInformation</code> object containing the game description
 * @specfield startClock_ the <code>Clock</code> representing the time from the reception of the START request before starting to play
 * @specfield playClock_ the <code>Clock</code> representing the time allotted to each move
 */
class RandomPlayer(private val gameId_ : String, private val reasoner_ : AbstractReasoner, private val role_ : GdlAtom, private val gameInformation_ : GameInfo, private val startClock_ : Int, private val playClock_ : Int) extends AbstractPlayer(gameId_, reasoner_, role_, gameInformation_, startClock_, playClock_)  {

	import epfl.algo.scarface.proof.Expr._
	import epfl.algo.scarface.util.Utils._
  
	// Specified by AbstractPlayer superclass
	override def think : Unit = {
		// typical of randomness :P
	}
	
	// Specified by AbstractPlayer superclass
	override def bestMove : Triple[Term, String, String] = 
		(groundFactToTerm(randomMove(role_.s_)), "", "")
	
}
