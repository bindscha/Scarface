package epfl.algo.scarface.player

import epfl.algo.scarface.gdl.GameInfo
import epfl.algo.scarface.gdl.Gdl._
import epfl.algo.scarface.proof.AbstractReasoner

import epfl.algo.scarface.util.Logging
import epfl.algo.scarface.util.Utils._

// TODO: refactor this so it takes enums as parameters for action. e.g. stateIs(enum) where enum \in {THINKING, STOPPED}
/**
 * An <code>AbstractPlayer</code> represents a basic Scarface player.
 *
 * @specfield gameId_ the identifier of the game being played
 * @specfield parser_ the GDL <code>Parser</code> used
 * @specfield reasoner_ the <code>AbstractReasoner</code> used to reason about games
 * @specfield role_ the <code>GdlAtom</code> representing the role of this player
 * @specfield gameInformation_ the <code>GameInformation</code> object containing the game description
 * @specfield startClock_ the <code>Clock</code> representing the time from the reception of the START request before starting to play
 * @specfield playClock_ the <code>Clock</code> representing the time allotted to each move
 */
abstract class AbstractPlayer protected (private val gameId_ : String, private val reasoner_ : AbstractReasoner, private val role_ : GdlAtom, private val gameInformation_ : GameInfo, private val startClock_ : Clock, private val playClock_ : Clock) extends CognizingAgent(gameId_, reasoner_) with Logging {

  import epfl.algo.scarface.proof.EmptyState
  import epfl.algo.scarface.proof.Expr._
  import epfl.algo.scarface.util.Utils._
  
  import scala.util.Random
  
  protected val roles_ : List[Object] = gameInformation_.roles

  // Same as elsewhere, must call toString on r so equals is correct
  protected val roleIndex_ : Int = roles_ findIndexOf (role_.s_ == _.symbol_)
  
  protected val QUERY_LEGAL : Query = VariableFact(Object('legal), List(Object(role_.s_), Var('X)))
  protected val QUERY_GOAL : Query = VariableFact(Object('goal), List(Object(role_.s_), Var('X)))
  
  protected var state_ : State = new State(gameInformation_.init)
  
  protected val random_ : Random = new Random
  
  /**
   * @return the game identifier
   */
  def gameId : String = gameId_
  
  def playClock : Int = playClock_
  
  def startClock : Int = startClock_
  
  /**
   * Updates the player with the latest moves.
   */
  def update(_newMoves : GdlList) : Unit = {
	  debug("[Game " + gameId_ + "] Updated moves" + (if(_newMoves != null) ": " + _newMoves.toString else "."))
	  
	  val parsedMoves : List[GroundFact] = parseMoves(_newMoves);
	  
	  state_ = doUpdate(state_, parsedMoves)
  }
  
  /**
   * @return a move to play in the next turn. Format: (move, explanation, taunt).
   */
  def play : Triple[GdlTerm, Explanation, Taunt] = {
	debug("[Game " + gameId_ + "] Move requested!")
	var move : Triple[Term, Explanation, Taunt] = bestMove
	if(move == null) {
	  warn("[Game " + gameId_ + "] Received null move from player!")
	  move = (null, "", "")
	}
	val moveGdl = if(move._1 == null) GdlList(Nil) else termToGdlTerm(move._1)
	debug("[Game " + gameId_ + "] Replied with move: " + moveGdl + ", explanation: " + move._2 + ", taunt: " + move._3 + ".")
	(moveGdl, move._2, move._3)
  }
  
  override def think : Unit = {
	  
  }
  
  override def terminate : Unit = {
	
  }
  
  /**
   * @return the best move to play in the next turn. Format: (move, explanation, taunt).
   */
  protected def bestMove : Triple[Term, Explanation, Taunt]
  
  /**
   * Transforms a list of moves into a list of ground facts.
   */
  protected def parseMoves(_moves : GdlList) : List[GroundFact] = {
	 if(_moves == null)
	 	  return Nil
	 
	 if(_moves.l_.size != roles_.size)
		 error("[Game " + gameId_ + "] Moves size is not the same as the roles size!")
		 
	 (for(val i <- 0 until _moves.l_.size; i < roles_.size) yield GroundFact(Object('does), List(roles_(i), gdlTermToTerm(_moves.l_(i))))).toList
  }
  
  protected def update(_state : State, _moves : List[GroundFact]) : State = {
	  val moves : List[GroundFact] = (for(val i <- 0 until _moves.size; i < roles_.size) yield GroundFact(Object('does), List(roles_(i), groundFactToTerm(_moves(i))))).toList
	  doUpdate(_state, moves)
  }
  
  private def doUpdate(_state : State, _moves : List[GroundFact]) : State = {
	  if(_moves.length > 0) {
	 	  val statee = _state validate _moves
		  
		  debug("[Game " + gameId_ + "] Updating game state...")
		  debug("[Game " + gameId_ + "] Previous game state: " + "\n\t" + statee)
		 	  
		  val newFacts : List[GroundFact] = (answers(QUERY_NEXT, statee)._2) map { case GroundFact(Object(s), l) if s == 'next => GroundFact(Object('true), l) }

		  val newState : State = new State(newFacts)
		  
		  debug("[Game " + gameId_ + "] New game state: " + "\n\t" + newState)
		  
		  newState
	  } else {
	 	  warn("[Game " + gameId_ + "] Update called with empty list of moves!")
	 	  _state
	  }
  }
  
  implicit def state : State = state_
  
  protected def terminal(_state : State) : Boolean = 
	  answers(QUERY_TERMINAL, _state)._1
	  
  protected def goal(_role : Symbol)(implicit _state : State) : Score = answer(makeGoalQuery(_role), _state)._2 match {
	  case Some(goal) => 
	    try {
		  (goal.terms_(1)).toString.toDouble
		} catch {
		  case e => Double.MinValue
		}
	  case None => 
	    error("[Game " + gameId_ + "] Goal is null!")
	    Double.MinValue
  } 
  
  protected def randomMove(_role : Symbol)(implicit _state : State) : Action = randomAnswer(makeLegalQuery(_role), _state)._2 match {
	  case Some(move) => 
	   cleanLegal(move)
	   /*move match {
	  	   case GroundFact(Object('legal), player :: Object(name) :: Nil) => GroundFact(Object(name), Nil)
	  	   case GroundFact(Object('legal), player :: Function(name, arguments) :: Nil) => GroundFact(Object(name), arguments)
	  	   case _ => throw new RuntimeException("Reasoner replied with " + move + " to the query legal(?X)!")
	   }*/
	  case None => null
  }
  
  protected def cleanLegal(_move : Action) : Action = _move match {
	  case GroundFact(Object('legal), player :: Object(name) :: Nil) => GroundFact(Object(name), Nil)
	  case GroundFact(Object('legal), player :: Function(name, arguments) :: Nil) => GroundFact(Object(name), arguments)
	  case _ => throw new RuntimeException("Reasoner replied with " + _move + " to the query legal(?X)!")
  }
  
  protected def makeLegalQuery(_role : Symbol) : Fact = 
	  VariableFact(Object('legal), List(Object(_role), Var('X)))
	  
  protected def makeGoalQuery(_role : Symbol) : Fact = 
	  VariableFact(Object('goal), List(Object(_role), Var('X)))
}
