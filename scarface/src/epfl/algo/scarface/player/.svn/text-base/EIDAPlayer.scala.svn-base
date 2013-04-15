package epfl.algo.scarface.player

import epfl.algo.scarface.gdl.Gdl._
import epfl.algo.scarface.gdl.{Parser, GameInfo}
import epfl.algo.scarface.proof.AbstractReasoner

/**
 * Player using Enhanced Iterative Deepening A* (EIDA).
 */
class EIDAPlayer(private val gameId_ : String, private val reasoner_ : AbstractReasoner, private val role_ : GdlAtom, private val gameInformation_ : GameInfo, private val startClock_ : Int, private val playClock_ : Int) extends AbstractPlayer(gameId_, reasoner_, role_, gameInformation_, startClock_, playClock_)  {
	
	import scala.collection.mutable.HashSet
	
	import epfl.algo.scarface.util.Utils._
    
	import epfl.algo.scarface.proof.Expr._
	
	type Heuristic = (State => Score)
	
	// Path to goal (in correct order)
	private var solution_ : List[Action] = Nil
	
	private val visited_ : HashSet[Long] = new HashSet[Long]
	
	private val heuristic_ : Heuristic = (x : State) => 0.0
		
	info("[Game " + gameId_ + "] Player type is EIDAPlayer")
	override def think : Unit = {
		var solution : List[Action] = Nil
		var costLimit : Score = 1 //TODO: improve this...
		
		while(true) {
			info("[Game " + gameId_ + "] Thinking for one more iteration...")
				
			visited_ clear()
			visited_ += state_.zHash
			
			// Perform search
			val result = search(state_, 0, costLimit, Nil)
				
			solution = result._1
			costLimit = result._2
				
			info("[Game " + gameId_ + "] Raised cost limit to " + costLimit)
				
			synchronized {
				if(!solution.isEmpty) {
					solution_ = solution.reverse
					info("[Game " + gameId_ + "] Found solution! " + solution_)
					
					return
				}
			}
		}
	}
	
	override def update(_newMoves : GdlList) : Unit = _newMoves match {
		case GdlList(move) => 
			solution_ = solution_ match {
				case m :: rest => rest
				case _ => solution_
			}
		case _ => 
			throw new RuntimeException("Received update with more than one move on a single-player game!")
	}
	
	override def bestMove : Triple[Term, String, String] = 
		solution_ match {
			case move :: rest => 
				(groundFactToTerm(move), "", "")
			case _ => 
				(groundFactToTerm(randomMove(role_.s_)), "", "")
		}
		
	
	// Throughout the search, _path is reversed (i.e. last move first)
	private def search(_state : State, _startCost : Score, _costLimit : Score, _path : List[Action]) : Pair[List[Action], Score] = {
		
		def make(_move : Action) : State = {
			val (state : State) = update(_state, List(_move))
			state
		}
		
		debug("[Game " + gameId_ + "] Search. State: " + _state)
		debug("[Game " + gameId_ + "] Search. Start cost: " + _startCost)
		debug("[Game " + gameId_ + "] Search. Cost limit: " + _costLimit)
		debug("[Game " + gameId_ + "] Search. Path: " + _path)
		
		val minimumCost : Score = _startCost + heuristic_(_state)
		debug("[Game " + gameId_ + "] Search. Minimum cost: " + _state)
		
		if(minimumCost > _costLimit)
			return (Nil, minimumCost)
			
		if(goal(role_.s_)(_state) == 100)
			return (_path, _costLimit)
			
		var nextCostLimit : Score = Double.MaxValue
		
		val moves : List[Action] = answers(QUERY_LEGAL, _state)._2 map cleanLegal
		
		for(val move : Action <- moves) {
			val state : State = make(move)
			if(!(visited_ contains state.zHash)) {
				visited_ += state.zHash
				val newStartCost = _startCost + 1
				val (solution : List[Action], newCostLimit : Score) = search(state, newStartCost, _costLimit, move :: _path)
				if(solution != Nil)
					return (solution, newCostLimit)
				nextCostLimit = if (nextCostLimit < newCostLimit) nextCostLimit else newCostLimit
			}
		}
		
		return (Nil, nextCostLimit)
	}
		
}
