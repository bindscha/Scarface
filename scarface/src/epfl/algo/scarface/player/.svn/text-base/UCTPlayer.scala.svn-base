package epfl.algo.scarface.player

import epfl.algo.scarface.gdl.Gdl._
import epfl.algo.scarface.gdl.{Parser, GameInfo}
import epfl.algo.scarface.proof.AbstractReasoner

class Visits {
	
	import scala.collection.mutable.{Map, HashMap}
	
	import epfl.algo.scarface.util.Utils._
	
	private val N1 : Map[Long, NumVisits] = new HashMap[Long, NumVisits]
	private val N2 : Map[Long, Map[Action, NumVisits]] = new HashMap[Long, Map[Action, NumVisits]]
	
	def ++(_state : State) : Unit = 
		N1 += ((_state.zHash, (N1 getOrElse (_state.zHash, 0)) + 1))
	
	def ++(_state : State, _action : Action) : Unit = 
		N2 get _state.zHash match {
		case Some(map) => 
			map get _action match {
				case Some(score) => map += ((_action, score + 1))
				case None => map += ((_action, 1))
			}
		case None =>
			N2 += ((_state.zHash, new HashMap[Action, NumVisits] + ((_action, 1))))
			
	}
	
	def visits(_state : State) : NumVisits = N1 get _state.zHash match {
		case Some(n) => n
		case None => error("State " + _state + " is not in the database of visited states!")	
	}

	def visits(_state : State, _action : Action) : NumVisits = N2 get _state.zHash match {
		case Some(map) =>
			map get _action match {
				case Some(n) => n
				case None => error("Action " + _action + " is not in the database of taken actions for state " + _state + "!")
		}
		case None => error("State " + _state + " is not in the database of visited states!")	
	}	
	
}

class StateActionTable {
	
	import scala.collection.mutable.{Map, HashMap}
	
	import epfl.algo.scarface.util.Utils._
	
	private val Q : Map[Long, Map[Action, Double]] = new HashMap[Long, Map[Action, Score]]
	
	def score(_state : State, _action : Action): Option[Score] = 
		Q get _state.zHash match {
			case Some(map) => map get _action
			case None => None
		}
		//(Q getOrElseUpdate (_state, new HashMap[Action, Score])) getOrElse (_action, Double.MinValue)
	
	def scoreIs(_state : State, _action : Action, _score : Score) : Unit = 
		(Q getOrElseUpdate (_state.zHash, new HashMap[Action, Score])) += ((_action, _score))
		
	def best(_state : State) : Pair[Action, Score] = 
		Q get _state.zHash match {
			case Some(map) => 
				map max Ordering[Score].on[(_, Score)](_._2)
			case None => (null, Double.MinValue)
		}
	
}

class SearchInterruptedException extends Exception

/**
 * Player using Monte Carlo methods with Upper Confidence Trees (MC with UCT).
 */
class UCTPlayer(private val gameId_ : String, private val reasoner_ : AbstractReasoner, private val role_ : GdlAtom, private val gameInformation_ : GameInfo, private val startClock_ : Int, private val playClock_ : Int) extends AbstractPlayer(gameId_, reasoner_, role_, gameInformation_, startClock_, playClock_)  {

	import scala.collection.mutable.{Map, HashMap}
	
	import epfl.algo.scarface.proof.Expr._
	import epfl.algo.scarface.util.Utils._
	
	// Discount factor
	private val GAMMA : Double = 0.99
	
	// UCT parameter
	private val CP : Double = 40
	
	private val Q : List[StateActionTable] = for(val r <- roles_) yield new StateActionTable
	
	private val N : List[Visits] = for(val r <- roles_) yield new Visits

	private var bestMove_ : Option[Action] = None
	private var bestScore_ : Score = Double.MinValue
	
	private var updated_ : Boolean = false
	
	info("[Game " + gameId_ + "] Player type is UCTPlayer")

	override def think : Unit = 
		while(true) {
			debug("[Game " + gameId_ + "] Thinking for one more iteration...")
			
			try {
				if(updated_)
					synchronized {
						updated_ = false
					}
				
				val (move, scores) = search(state)
				
				synchronized {	
					if(!updated_ && scores(roleIndex_) > bestScore_) {
						info("[Game " + gameId_ + "] Found better move " + move + " with score " + scores(roleIndex_))
						bestScore_ = scores(roleIndex_)
						bestMove_ = move
					}
				}
			} catch {
				case e : SearchInterruptedException => 
			}
		}
	
	override def update(_newMoves : GdlList) : Unit = {
		synchronized {
			bestMove_ = None
			bestScore_ = Double.MinValue
			super.update(_newMoves)
			updated_ = true
		}
	}
	
	override def bestMove : Triple[Term, String, String] = synchronized {
		(updated_, bestMove_) match {
			case (false, Some(move)) => 
				(groundFactToTerm(move), "", "")
			case _ => 
				(groundFactToTerm(randomMove(role_.s_)), "", "")
		}
	}
	
	private def search(_state : State) : Pair[Option[Action], List[Score]] = {
		def make(_moves : List[Action]) : Pair[State, List[Score]] = {
			val state : State = update(_state, _moves)
			val rewards : List[Score] = (for(i <- 0 until roles_.size) yield 0.0).toList
			(state, rewards)
		}

		synchronized {
			if(updated_)
				throw new SearchInterruptedException
		}
		
		if(terminal(_state)) {
			(None, (for(i <- 0 until roles_.size) yield goal(roles_(i).symbol_)(_state)).toList)
		} else {
			var moves : Array[Action] = new Array[Action](roles_.size)
			for(i <- 0 until roles_.size) {
				val possibleMoves : List[Action] = (answers(makeLegalQuery(roles_(i).symbol_), _state)._2 map cleanLegal).toList
				moves(i) = selectAction(possibleMoves, _state, i)
			}
			
			val (state : State, rewards : List[Score]) = make(moves.toList)
				
			val searchRewards = search(state)._2
			
			val qs : List[Score] = (for(i <- 0 until roles_.size) yield rewards(i) + GAMMA * searchRewards(i)).toList
			
			for(i <- 0 until roles_.size)
				updateValue(_state, moves(i), qs(i), i)
				
			(Some(moves(roleIndex_)), qs)
		}
	}
	
	private def selectAction(_moves : List[Action], _state : State, _roleIndex : Int) : Action = {
		var value : Double = Double.MinValue
		var currentValue : Double = 0.0
		var randMax : List[Action] = Nil
		
		for(val move <- _moves) {
			N(_roleIndex) ++ _state
			N(_roleIndex) ++ (_state, move)
			
			currentValue = Q(_roleIndex).score(_state, move) match {
				case Some(q) => q + CP * Math.sqrt(Math.log(N(_roleIndex) visits (_state, move)) / (N(_roleIndex) visits _state))
				case None => Double.MaxValue
			}
			
			if(currentValue == value) {
				randMax = move :: randMax
			}
			else if(currentValue > value) {
				value = currentValue
				randMax = List(move)
			}
		}
		
		randMax(random_ nextInt randMax.size)
	}
	
	private def updateValue(_state : State, _move : Action, _q : Score, _roleIndex : Int) : Unit = {
		val Qprev = Q(_roleIndex).score(_state, _move) match {
			case Some(q) => q
			case None => 0.0
		}
		Q(_roleIndex) scoreIs (_state, _move, Qprev + 1.0 / N(_roleIndex).visits(_state, _move) * (_q - Qprev))
	}
	
}
