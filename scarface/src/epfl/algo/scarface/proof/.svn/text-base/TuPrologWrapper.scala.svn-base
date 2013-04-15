package epfl.algo.scarface.proof

import alice.{ tuprolog => P }

import epfl.algo.scarface.proof.{ Expr => E }
	
class TuPrologWrapper(private val nonVolatileState_ : State, private val rules_ : List[E.Rule]) extends AbstractLogic(nonVolatileState_, rules_) {
	
	private val engine_ : P.Prolog = new P.Prolog
	
	private val theory_ : P.Theory = new P.Theory(new P.Struct(transform(nonVolatileState_.groundFacts ::: rules_) toArray))
	
	engine_ setTheory theory_
	
	def apply(_fact : E.Fact, _state : State) : Pair[Boolean, Stream[E.GroundFact]] = {
		val newFactsArray : Array[P.Term] = transform(_state.groundFacts) toArray;
		val newTheory = new P.Theory(new P.Struct(newFactsArray))
		
		engine_.clearTheory
		engine_ addTheory theory_
		
		engine_ addTheory newTheory
		
		convertStream(_fact, engine_)
	}
	
	private def transform(_expressions : List[E.PrologConvertible]) : List[P.Term] = 
		_expressions map transform
		
	private def transform(_expression : E.PrologConvertible) : P.Term = _expression match {
		case f : E.Fact => transformFact(f)
		case E.Rule(consequence, antecedents) => new P.Struct(":-", transformFact(consequence), transformConjunction(antecedents))
	}
	
	private def transformFact(_fact : E.Fact) : P.Struct = _fact match {
		case E.GroundFact(E.Object(s), l) => new P.Struct(s.name, transformTerms(l) toArray)
		case E.VariableFact(E.Object(s), l) if s == 'distinct => new P.Struct("\\=", transformTerms(l) toArray)
		case E.VariableFact(E.Object(s), l) => new P.Struct(s.name, transformTerms(l) toArray)
	}
	
	private def transformConjunction(_conjunction : E.Conjunction) : P.Term = (_conjunction.expressions_ map transformConjuct) match {
		case Nil => P.Term.TRUE
		case l :: Nil => l
		case l :: ls => (l /: ls)(new P.Struct(",", _, _))
	}
	
	private def transformDisjunction(_disjunction : E.Disjunction) : P.Term = (_disjunction.expressions_ map transformConjuct) match {
		case Nil => P.Term.TRUE
		case l :: Nil => l
		case l :: ls => val a = (l /: ls)(new P.Struct(";", _, _))
		a
	}
	
	private def transformTerms(_terms : List[E.Term]) : List[P.Term] = 
		_terms map transformTerm
		
	private def transformTerm(_term : E.Term) : P.Term = _term match {
		//case E.Object(s) if isInt(s.name) => new P.Int(s.name.toInt)
		case E.Object(s) => new P.Struct(s.name)
		case E.Var(s) => new P.Var(s.name)
		case E.Function(s, arguments) => new P.Struct(s.name, transformTerms(arguments) toArray)
	}
	
	private def isInt(_s : String) : Boolean = try {
		val s = if(_s contains "'") _s substring((_s indexOf "'") + 1, _s lastIndexOf "'") else _s
		s.toInt
		true
	} catch {
		case _ => false
	}
	
	private def transformConjuct(_conjuct : E.Expr) : P.Term = _conjuct match {
		case f : E.Term => transformTerm(f)
		case f : E.Fact => transformFact(f)
		case E.Negation(e) => new P.Struct("not", transformConjuct(e))
		case d : E.Disjunction => transformDisjunction(d)
	}
	
	private def convertStream(_fact : E.Fact, _engine : P.Prolog) : Pair[Boolean, Stream[E.GroundFact]] = {
		val goal = transformFact(_fact)
		var answer = _engine solve goal
		/*println(answer)
		while(engine_ hasOpenAlternatives) {
			answer = engine_.solveNext
			println(answer)
		}*/
		(answer.isSuccess, Stream.cons(transformBackFact(goal, answer), convertStream0(goal, _engine)))
		//Stream.Empty
	}
	
	private def convertStream0(_fact : P.Struct, _engine : P.Prolog) : Stream[E.GroundFact] = 
		if(_engine hasOpenAlternatives) {
			val answer = _engine.solveNext
			if(answer isSuccess)
				Stream.cons(transformBackFact(_fact, answer), convertStream0(_fact, _engine))
			else
				Stream.Empty
		} else {
			Stream.Empty
		}
	
	private def transformBackFact(_fact : P.Term, _answer : P.SolveInfo) : E.GroundFact = transformBackTerm(_fact, _answer) match {
		case E.Object(s) => E.GroundFact(E.Object(s), Nil)
		case E.Function(s, arguments) => E.GroundFact(E.Object(s), arguments)
	}
	
	private def transformBackTerm(_term : P.Term, _answer : P.SolveInfo) : E.Term = 
		if(_term.isVar) {
			val name = _term.asInstanceOf[P.Var].toString
			if(name == name.toUpperCase && !isInt(name))
				transformBackTerm(_answer.getTerm(name), _answer)
			else if(isInt(name))
				E.Object(Symbol(if(name contains "'") name substring((name indexOf "'") + 1, name lastIndexOf "'") else name))
			else
				E.Object(Symbol(name))
		} else if(_term.isNumber) {
			E.Var(Symbol((_term.asInstanceOf[P.Number] intValue) toString)) // TODO: handle other cases
		} else {
			val struct = _term.asInstanceOf[P.Struct]
			if(struct.getArity > 0)
				E.Function(Symbol(struct.getName), (for (i <- 0 until struct.getArity) yield transformBackTerm(struct.getArg(i), _answer)) toList)
			else
				E.Object(Symbol(struct.getName))
		}

}