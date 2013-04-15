package epfl.algo.scarface.test
 
import junit.framework._
import org.junit.Assert._

/**
 * Tests for the transform method in l3.CPSDataRepresenter
 */
class LogicTest2 extends TestCase {
 
	import epfl.algo.scarface.gdl.Gdl._
	import epfl.algo.scarface.gdl.Parser
	import epfl.algo.scarface.gdl.GameInfo
	import epfl.algo.scarface.proof.Expr._
	import epfl.algo.scarface.proof.State
	import epfl.algo.scarface.proof._
	
    /* Tests the behavior on simple cases */
    def testSimpleCases = {
		
		import alice.tuprolog.{Prolog, Theory, SolveInfo, Term, Struct, Var}
		
		val prolog = new Prolog
		//val t = Term.createTerm("next(test(X, Y)) :- cond(X), X \\= a ; cond(Y), Y \\= b")
		val t = Term.createTerm("next(test(X, Y)) :- cond(1, X), cond(2, Y)")
		val a = new Array[Term](3)
		a(0) = Term.createTerm("cond(1, a)")
		a(1) = Term.createTerm("cond(2, b)")
		a(2) = new Struct("c")
		val b = new Array[Term](1)
		b(0) = t
		//b(0) = new Struct("condd", new Struct("a"))
		//b(0) = new Struct(":-", new Struct("next", new Struct("test", new Var("X"))), new Struct("cond", new Var("X")))
		val theory = new Theory(new Struct(a))//new Theory(new java.io.FileInputStream("C:\\Users\\Phoenix\\Desktop\\hanoi.pl"))//new Struct(a))
		theory append new Theory(new Struct(b))
		val goal = new Struct("next", new Var("X"))
		println(theory)
		prolog.setTheory(theory)
		val answer = prolog.solve(goal)
		println(answer.getTerm("X"))
		
    	/*val parser : Parser = new Parser(new java.io.FileInputStream("bin/epfl/algo/scarface/test/hanoi.gdl"))
	    val game : GdlList = parser.game()
	    
	    val gi : GameInfo = new GameInfo(game)
    	val logicFactory = new GenericLogicFactory[TuPrologWrapper]("TuPrologWrapper")("epfl.algo.scarface.proof")
    	
	    val reasoner = new SimpleReasoner(logicFactory.logicNew(gi))
    	
    	val action : GroundFact = GroundFact(Object('does), List(Object('player), Function('puton, List(Object('disc1), Object('pillar2)))))
    	
    	val state = new State(gi.init)
    	
    	val question : Fact = VariableFact(Object('legal), List(Object('player), Var('X)))
    	
    	reasoner.answers(question, state) foreach println*/
    }
  
}