package epfl.algo.scarface.proof

import scala.collection.Map

import epfl.algo.scarface.proof.Expr._

// TODO: possibly change datastructure...
// Tradeoff between lookup (e.g. find(init)) and zHash
// Actually, if priority map, inits should be together with the ordering I believe
// => Go for this whenever you can

object EmptyState extends State(Set[GroundFact]())

class State private (val database_ : Map[Object, Set[GroundFact]]) {
	
	import scala.collection.immutable.TreeMap
	
	def this(_s : State) = this(_s.database_)
	
	def this(_facts : Set[GroundFact]) = this {
		(_facts groupBy ((f : GroundFact) => f.relation_))
	}
	
	def this(_facts : Iterable[GroundFact]) = this(_facts toSet)
	
	def this(_fact : GroundFact) = this(List(_fact))
	
	def this(_facts : GroundFact*) = this(_facts)
	
	def validate(_f : GroundFact) : State =
		new State(database_ + ((_f.relation_, ((database_ getOrElse (_f.relation_, Set())) + _f))))
	
	def validate(ls : List[GroundFact]) : State = {
		var newDB = database_
		for(val f <- ls)
			newDB += ((f.relation_, ((newDB getOrElse (f.relation_, Set())) + f)))
		new State(newDB)
	}
	
	def invalidate(f : GroundFact) : State =
		new State(database_ + ((f.relation_, ((database_ getOrElse (f.relation_, Set())) - f))))
	
	def groundFacts : List[GroundFact] = 
		database_.values.flatten.toList.sorted(groundFactOrdering)
	
	def zHash : Long = {
		var key : Long = 0L
		var shift : Int = 0
		for (val c <- groundFacts) {
			key = key ^ ror(Expr.zHash(c), shift)
			shift = (shift + 1) & shiftMask
		}
		key
	}
	
	override def toString : String = {
		"State (" + zHash + "): \n\t" + (database_ mkString "\n\t")
	}
	
}