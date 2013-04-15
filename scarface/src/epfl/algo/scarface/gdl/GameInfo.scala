package epfl.algo.scarface.gdl

import epfl.algo.scarface.gdl.Gdl._
import epfl.algo.scarface.proof.Expr._

object Symbols {
	val role = 'role
	val init = 'init
	val tru = 'true
	val does = 'does
	val next = 'next
	val legal = 'legal
	val goal = 'goal
	val terminal = 'terminal
	val distinct = 'distinct
	
	val or = 'or
	val and = 'and
	val not = 'not
	val impl = '<=
}

class GameInfo(private val game_ : GdlList) {

	import scala.collection.immutable.{Map, HashMap, Set, HashSet}
	
	import Symbols._
	
	private val SYMBOLS_TO_REMOVE : List[Symbol] = List('init, 'true, 'next, 'legal, 'does)
	
	private var relations_ : Map[Symbol, Arity] = new HashMap[Symbol, Arity]
	private var functions_ : Set[Symbol] = new HashSet[Symbol]
	private var objects_ : Set[Symbol] = new HashSet[Symbol]
	
	private var rules_ : Map[Symbol, Set[Rule]] = new HashMap[Symbol, Set[Rule]]
	private var groundFacts_ : Map[Symbol, Set[GroundFact]] = new HashMap[Symbol, Set[GroundFact]]
	
	private var roles_ : List[Object] = Nil
	
	private val expressions_ : List[Expr] = examine(game_.l_)
	
	private val initFacts_ : List[GroundFact] = findInit
	private val staticFacts_ : List[GroundFact] = findStaticFacts
	
	private val staticRelations_ : Map[Symbol, Set[GroundFact]] = findStaticRelations
	
	private val rulesList_ : List[Rule] = ((rules_ values) flatten) toList
	
	private val legalRules_ : List[Rule] = findLegalRules	
	private val nextRules_ : List[Rule] = findNextRules
	
	// Insert reserved relations
	relNew('role, 1)
	relNew('init, 1)
    relNew('true, 1)
    relNew('does, 2)
    relNew('next, 1)
    relNew('legal, 2)
    relNew('goal, 2)
    relNew('terminal, 0)
    relNew('distinct, 2)
    		
    def relations : Map[Symbol, Arity] = relations_
    
    def functions : Set[Symbol] = functions_
    
    def objects : Set[Symbol] = objects_
    
    def rules : Map[Symbol, Set[Rule]] = rules_
    
    def groundFacts : Map[Symbol, Set[GroundFact]] = groundFacts_
    
    def roles : List[Object] = roles_
    
    def expressions : List[Expr] = expressions_
    
    def staticRelations : Map[Symbol, Set[GroundFact]] = staticRelations_
    	
    def initFacts : List[GroundFact] = initFacts_
    
    def init : List[GroundFact] = (groundFacts_('init) toList) map { case GroundFact(Object(s), l) if s == 'init => GroundFact(Object('true), l) } 
    
    def staticFacts : List[GroundFact] = staticFacts_
    
    def rulesList : List[Rule] = rulesList_
    
    def legalRules : List[Rule] = legalRules_
    
    def nextRules : List[Rule] = nextRules_
    
    private def examine(_list : List[GdlTerm]) : List[Expr] = _list match {
    	case Nil => Nil
    	case GdlAtom(s) :: xs => 
    		relNew(s, 0)
    		GroundFact(Object(s), Nil) :: examine(xs)
    	case GdlList(GdlAtom(s) :: (l : List[GdlTerm])) :: xs if s == impl => 
    		val rule : Rule = examineRule(_list.head)
    		ruleNew(ruleSymbol(rule), rule)
			rule :: examine(xs)
		case GdlList(GdlAtom(s) :: (l : List[GdlTerm])) :: xs => 
			val groundFact : GroundFact = examineRelation(_list.head) match {
				case f : GroundFact => f
				case _ => throw new RuntimeException(_list.head + " is not a ground fact!")
			}
			groundNew(s, groundFact)
			
			// Check if we have a role and if so add it to the list
			groundFact match {
				case GroundFact(Object(s), List(Object(player))) if s == role => 
					roles_ = roles_ ::: List(Object(player))
				case _ => // Do nothing
			}
				
    		groundFact :: examine(xs)
    }
    
    private def examineRule(_term : GdlTerm) : Rule = _term match {
    	case GdlList(GdlAtom(s) :: l :: ls) if s == impl =>
    		val consequence : Fact = examineRelation(l) match {
    			case f : Fact => f
    			case _ => throw new RuntimeException(_term + " is not a valid rule (head is not a fact)!")
    		}
    		val antecedents : Conjunction = Conjunction(ls map examineRelation)
    		Rule(consequence, antecedents)
    	case _ => throw new RuntimeException("ExamineRule must be called with a term corresponding to a rule!")
    }
    
    private def examineRelation(_term : GdlTerm) : Expr = _term match {
    	case GdlAtom(s) => 
    		relNew(s, 0)
    		makeFact(_term) match {
    			case f : GroundFact => f
				case _ => throw new RuntimeException(_term + " is not a ground fact!")
			}
    	case GdlList(GdlAtom(s) :: List(t)) if s == not => 
    		Negation(examineRelation(t))
    	case GdlList(GdlAtom(s) :: l) if s == or => 
    		Disjunction(l map examineRelation)
    	case GdlList(GdlAtom(s) :: l) => 
    		relNew(s, l size)
    		l foreach examineTerm
    		makeFact(_term)
    	case _ => throw new RuntimeException("ExamineRelation must be called with a term corresponding to a relation!")
    }
    
    private def examineTerm(_term : GdlTerm) : Unit = _term match {
    	case GdlAtom(s) => 
    		objNew(s)
    	case GdlVar(s) =>
    		// Do nothing
    	case GdlList(GdlAtom(s) :: l) => 
    		funNew(s)
    		l foreach examineTerm
    }
    
    private def makeFact(_term : GdlTerm) : Fact = _term match {
    	case GdlAtom(s) => 
    		GroundFact(Object(s), Nil)
    	case GdlList(GdlAtom(s) :: l) if hasVariables(_term) => 
    		VariableFact(Object(s), l map gdlTermToTerm)
    	case GdlList(GdlAtom(s) :: l) => 
    		GroundFact(Object(s), l map gdlTermToTerm)
    }
    
    private def hasVariables(_tree : GdlTerm) : Boolean = _tree match {
		case GdlAtom(_) => false
		case GdlVar(_) => true
		case GdlList(l) => l exists hasVariables
	}

    private def gdlTermToTerm(_term : GdlTerm) : Term = _term match {
		case GdlAtom(s) => Object(s)
		case GdlVar(s) => Var(s)
		case GdlList(GdlAtom(s) :: l) => Function(s, l map gdlTermToTerm)
	}

	private def dropSymbols[A](_filter : (Symbol, A) => A)(_list : Iterable[A]) : Iterable[A] = {
		var tmpList : Iterable[A] = _list
		for(s <- SYMBOLS_TO_REMOVE)
			tmpList = tmpList map (e => _filter(s, e))
		tmpList
	}
	
	private def drop(_symbol : Symbol, _expressions : List[Expr]) : List[Expr] =
		_expressions map (e => drop(_symbol, e))
	
	private def drop(_symbol : Symbol, _expr : Expr) : Expr = _expr match {
		case Function(s, List(t)) if s == _symbol => t
		case f : Fact => dropFact(_symbol, f)
		case r : Rule => dropRule(_symbol, r)
		case Negation(expressions) => Negation(drop(_symbol, expressions))
		case _ => _expr
	}
	
	private def dropRule(_symbol : Symbol, _rule : Rule) : Rule = 
		Rule(dropFact(_symbol, _rule.consequence_), dropConjunction(_symbol, _rule.antecedents_))
	
	private def dropConjunction(_symbol : Symbol, _conjunction : Conjunction) : Conjunction = 
		Conjunction(_conjunction.expressions_ map (e => drop(_symbol, e)))
	
	private def dropFact(_symbol : Symbol, _fact : Fact) : Fact = _fact match {
		case gf : GroundFact => dropGroundFact(_symbol, gf)
		case vf : VariableFact => dropVariableFact(_symbol, vf)
	}
	
	private def dropGroundFact(_symbol : Symbol, _fact : GroundFact) : GroundFact = _fact match {
		case GroundFact(Object(s), List(Function(fs, args))) if _symbol == s => GroundFact(Object(fs), args)
		case GroundFact(Object(s), List(_, Function(fs, args))) if _symbol == s => GroundFact(Object(fs), args)
		case _ => _fact
	}
	
	private def dropVariableFact(_symbol : Symbol, _fact : VariableFact) : VariableFact = _fact match {
		case VariableFact(Object(s), List(Function(fs, args))) if _symbol == s => VariableFact(Object(fs), args)
		case VariableFact(Object(s), List(_, Function(fs, args))) if _symbol == s => VariableFact(Object(fs), args)
		case _ => _fact
	}

	private def findInit : List[GroundFact] = 
		dropSymbols(dropGroundFact)(groundFacts_('init)) toList
	
	private def findStaticFacts : List[GroundFact] = 
		dropSymbols(dropGroundFact)(((groundFacts_ - 'init) values) flatten) toList
	
	private def findLegalRules : List[Rule] = 
		dropSymbols(dropRule)(((rules_ - 'terminal - 'next) values) flatten) toList
	
	private def findNextRules : List[Rule] = 
		dropSymbols(dropRule)(((rules_ - 'terminal - 'legal) values) flatten) toList
	
	private def findStaticRelations : Map[Symbol, Set[GroundFact]] = 
		groundFacts_ filter { case (_, GroundFact(_, Nil)) => false ; case _ => true } filter (gf => !gf._2 .isEmpty) 
	
	//
	// Helper methods
	//
	
	private def ruleSymbol(_rule : Rule) : Symbol = _rule match {
		case Rule(GroundFact(Object(s), _), _) => s
		case Rule(VariableFact(Object(s), _), _) => s
	}
		
	private def relNew(_relation : Symbol, _arity : Arity) = {
		if(isFun(_relation) || isObj(_relation))
			throw new RuntimeException("Relation symbol " + _relation + " was already declared as a function or object symbol!")
		
		if(!(relations_ contains _relation))
			groundFacts_ += ((_relation, new HashSet[GroundFact]))
		relations_ += ((_relation, _arity))
	}
	
	private def funNew(_function : Symbol) = {
		if(isRel(_function) || isObj(_function))
			throw new RuntimeException("Function symbol " + _function + " was already declared as a relation or object symbol!")
		
		functions_ += _function
	}
	
	private def objNew(_object : Symbol) = {
		if(isRel(_object) || isFun(_object))
			throw new RuntimeException("Object symbol " + _object + " was already declared as a relation or function symbol!")
		
		objects_ += _object
	}
	
	private def ruleNew(_name : Symbol, _rule : Rule) = 
		rules_ += ((_name, (rules_ getOrElse(_name, new HashSet[Rule])) + _rule))
	
	private def groundNew(_name : Symbol, _ground : GroundFact) = 
		groundFacts_ += ((_name, (groundFacts_ getOrElse(_name, new HashSet[GroundFact])) + _ground))
		
	private def isRel(_symbol : Symbol) : Boolean = 
		relations_ contains _symbol
		
	private def isFun(_symbol : Symbol) : Boolean = 
		functions_ contains _symbol
		
	private def isObj(_symbol : Symbol) : Boolean = 
		objects_ contains _symbol
	
}