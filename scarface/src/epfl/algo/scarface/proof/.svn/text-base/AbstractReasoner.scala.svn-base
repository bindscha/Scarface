package epfl.algo.scarface.proof

import epfl.algo.scarface.proof.Expr._

abstract class AbstractReasoner (private val logic_ : AbstractLogic) {
	
	def answer(_fact : Fact, _state : State) : Pair[Boolean, Option[GroundFact]]
	
	def answers(_fact : Fact, _state : State) : Pair[Boolean, List[GroundFact]]
	
	def randomAnswer(_fact : Fact, _state : State) : Pair[Boolean, Option[GroundFact]]
	
}