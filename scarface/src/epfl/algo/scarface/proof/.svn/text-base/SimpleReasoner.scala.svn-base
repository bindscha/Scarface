package epfl.algo.scarface.proof

import epfl.algo.scarface.proof.Expr._

class SimpleReasoner(private val logic_ : AbstractLogic) extends AbstractReasoner(logic_) {
	
	import scala.util.Random
	
	private val random_ : Random = new Random
	
	override def answer(_fact : Fact, _state : State) : Pair[Boolean, Option[GroundFact]] = logic_(_fact, _state) match {
		case (result, Stream.Empty) => (result, None)
		case (result, Stream.cons(f, fs)) => (result, Some(f))
	}
	
	override def answers(_fact : Fact, _state : State) : Pair[Boolean, List[GroundFact]] = {
		val ans = logic_(_fact, _state)
		(ans._1, ans._2 toList)
	}
		
	override def randomAnswer(_fact : Fact, _state : State) : Pair[Boolean, Option[GroundFact]] = {
		val ans = logic_(_fact, _state)
		val (result : Boolean, facts : List[GroundFact]) = (ans._1, ans._2 toList)
		
		facts match {
			case Nil => (result, None)
			case _ => (result, Some(facts(random_.nextInt(facts size))))
		}
	}
	
}