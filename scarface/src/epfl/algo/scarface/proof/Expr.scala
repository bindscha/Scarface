package epfl.algo.scarface.proof

object Expr {

	abstract case class Expr {
		override def toString : String = this match {
			case Object(s) => s.name
			case Var(s) => "?" + s.name
			case Function(s, l) => s.name + "(" + (l mkString ",") + ")"
			case GroundFact(Object(s), l) => s.name + "(" + (l mkString ",") + ")"
			case VariableFact(Object(s), l) => s.name + "(" + (l mkString ",") + ")"
			case Negation(e) => "not(" + e + ")"
			case Disjunction(l) => 
				var out = "or("
				for(val e <- l) 
					out += "(" + e + ") "
				out + ")"
			case Conjunction(l) => 
				var out = ""
				for(val e <- l) 
					out += "(" + e + ") "
				out
			case Rule(c, a) => 
				"(<= " + c + " " + a + ")"
		}
	}
	
	abstract case class Term extends Expr
	case class Object(symbol_ : Symbol) extends Term
	case class Var(symbol_ : Symbol) extends Term
	case class Function(symbol_ : Symbol, arguments_ : List[Term]) extends Term
	
	abstract case class Fact extends Expr with PrologConvertible
	case class GroundFact(relation_ : Object, terms_ : List[Term]) extends Fact
	case class VariableFact(relation_ : Object, terms_ : List[Term]) extends Fact
	
	case class Negation(expression_ : Expr) extends Expr
	case class Conjunction(expressions_ : List[Expr]) extends Expr
	case class Disjunction(expressions_ : List[Expr]) extends Expr
	
	case class Rule(consequence_ : Fact, antecedents_ : Conjunction) extends Expr with PrologConvertible
	
	sealed abstract trait PrologConvertible
	
	type Arity = Int
	
	//
	// Utility methods
	//
	
	import epfl.algo.scarface.gdl.%
	
	val shiftInit : Int = 7
	val shiftMask : Int = 0x3F // 64
	
	implicit def objectOrdering : Ordering[Object] = Ordering[String].on[Object](_.symbol_.name)
	
	def ror(i : Long, n : Int) : Long =
		(i << (64-n)) | (i >>> n)
	
	def zHash(_groundFact : GroundFact) : Long = {
		var key : Long = %(_groundFact.relation_.symbol_)
		var shift : Int = shiftInit
		for (val l <- _groundFact.terms_) {
			key = key ^ ror(zHashTerm(l), shift)
			shift = (shift + 7) & shiftMask
		}
		key
	}
	
	private def zHashTerm(_term : Term) : Long = _term match {
		case Object(s) => %(s)
		case Function(s, args) => 
			var key : Long = %(s)
			var shift : Int = shiftInit
			for (val a <- args) {
				key = key ^ ror(zHashTerm(a), shift)
				shift = (shift + 7) & shiftMask
			}
			key
	}
	
	implicit def groundFactOrdering : Ordering[GroundFact] = new Ordering[GroundFact] {
		def compare(x : GroundFact, y : GroundFact) : Int = (x, y) match {
			case (GroundFact(h1, l1), GroundFact(h2, l2)) if compare(h1, h2) < 0 => -1
			case (GroundFact(h1, l1), GroundFact(h2, l2)) if compare(h1, h2) > 0 => 1
			case (GroundFact(_, l1), GroundFact(_, l2)) if l1.size < l2.size => -1
			case (GroundFact(_, l1), GroundFact(_, l2)) if l1.size > l2.size => 1
			case (GroundFact(_, l1), GroundFact(_, l2)) => 
				for((t1, t2) <- l1 zip l2) {
					val diff = compare(t1, t2)
					if(diff != 0)
						return diff
				}
				0
		}
		
		def compare(x : Term, y : Term) : Int = (x, y) match {
			case (Object(s1), Object(s2)) => %(s1) compare %(s2)
			case (Object(_), Var(_)) => -1
			case (Object(_), Function(_, _)) => -1
			case (Var(_), Object(_)) => 1
			case (Var(s1), Var(s2)) => %(s1) compare %(s2)
			case (Var(_), Function(_, _)) => -1
			case (Function(_, _), Object(_)) => 1
			case (Function(_, _), Var(_)) => 1
			case (Function(s1, l1), Function(s2, l2)) if (%(s1) compare %(s2)) < 0 => -1
			case (Function(s1, l1), Function(s2, l2)) if (%(s1) compare %(s2)) > 0 => 1
			case (Function(_, l1), Function(_, l2)) if l1.size < l2.size => -1
			case (Function(_, l1), Function(_, l2)) if l1.size > l2.size => 1
			case (Function(_, l1), Function(_, l2)) => 
				for((t1, t2) <- l1 zip l2) {
					val diff = compare(t1, t2)
					if(diff != 0)
						return diff
				}
				0
		}
	}
	
	def groundFactToTerm(groundFact : GroundFact) : Term = groundFact match {
		case GroundFact(Object(s), Nil) => Object(s)
		case GroundFact(Object(s), terms) => Function(s, terms)
	}
	
}