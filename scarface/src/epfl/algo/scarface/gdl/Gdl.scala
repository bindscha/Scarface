package epfl.algo.scarface.gdl

import scala.util.parsing.input.Positional

/**
 * GDL Grammar
 */
object Gdl {

	import epfl.algo.scarface.proof.{Expr => E}
	
	abstract class GdlTerm extends Positional {
		override def toString : String = this match {
			case GdlAtom(s) => s.name
			case GdlVar(s) => "?" + s.name
			case GdlList(l) => 
				"(" + (l mkString (" ")) + ")"
		}
	}
	
	case class GdlAtom(s_ : Symbol) extends GdlTerm
	case class GdlVar(s_ : Symbol) extends GdlTerm
	case class GdlList(l_ : List[GdlTerm]) extends GdlTerm

	def termToGdlTerm(e : E.Term) : GdlTerm = e match {
		case E.Object(s) => GdlAtom(s)
		case E.Var(s) => GdlVar(s)
		case E.Function(s, arguments) => GdlList(GdlAtom(s) :: (arguments map termToGdlTerm))
	}
	
	def gdlTermToTerm(t : GdlTerm) : E.Term = t match {
		case GdlAtom(s) => E.Object(s)
		case GdlVar(s) => E.Var(s)
		case GdlList(Nil) => throw new RuntimeException("Received empty list!")
		case GdlList(GdlAtom(s) :: Nil) => E.Object(s)
		case GdlList(GdlAtom(s) :: l) => E.Function(s, l map gdlTermToTerm)
	}
	
	def groundFactToGdlTerm(e : E.GroundFact) : GdlTerm = e match {
		case E.GroundFact(E.Object(s), Nil) => GdlAtom(s)
		case E.GroundFact(E.Object(s), terms) => GdlList(GdlAtom(s) :: (terms map termToGdlTerm))
	}
	
}
