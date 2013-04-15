package epfl.algo.scarface.player

import epfl.algo.scarface.proof.AbstractReasoner

/**
 * A <code>CognizingAgent</code> represents a cognizing agent. Cognizing agents are the basic model for players in Scarface.
 *
 * @specfield gameId_ the identifier of the game being played
 * @specfield parser_ the GDL <code>Parser</code> used
 * @specfield reasoner_ the <code>AbstractReasoner</code> used to reason about games
 */
abstract class CognizingAgent protected (private val gameId_ : String, private val reasoner_ : AbstractReasoner) extends Thread {

	import epfl.algo.scarface.proof.Expr._
	import epfl.algo.scarface.proof.State
	
	type Query = Fact
	
	protected val QUERY_TERMINAL : Query = GroundFact(Object('terminal), Nil)
    protected val QUERY_NEXT : Query = VariableFact(Object('next), List(Var('X)))
	
	/**
	 * @return the <code>AbstractReasoner</code> object used.
	 */
	def reasoner : AbstractReasoner = reasoner_
	
	// Specified by Thread superclass
	override def run : Unit = 
		think
	
    /**
     * Terminates the player.
     */
	def terminate : Unit
	
	/**
	 * Start the player's thinking process.
	 */
	protected def think : Unit
	
	/**
	 * @return a <code>GroundFact</code> representing a possible answer to the query in the given proof context.
	 */
	protected def answer(_query : Query, _state : State) : Pair[Boolean, Option[GroundFact]] = synchronized {
		reasoner_ answer (_query, _state)
	}
	
	/**
	 * @return a <code>List[GroundFact]</code> representing all possible answers to the query in the given proof context.
	 */
	protected def answers(_query : Query, _state : State) : Pair[Boolean, List[GroundFact]] = synchronized {
		reasoner_ answers (_query, _state)
	}
	
	/**
	 * @return a <code>GroundFact</code> representing a random possible answer to the query in the given proof context.
	 */
	protected def randomAnswer(_query : Query, _state : State) : Pair[Boolean, Option[GroundFact]] = synchronized {
		reasoner_ randomAnswer (_query, _state)
	}
	
}
