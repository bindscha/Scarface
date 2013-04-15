package epfl.algo.scarface.proof

import epfl.algo.scarface.proof.Expr._

abstract class AbstractLogic(private val nonVolatileState_ : State, private val rules_ : List[Rule]) extends ((Fact, State) => Pair[Boolean, Iterable[GroundFact]]) {
	
}