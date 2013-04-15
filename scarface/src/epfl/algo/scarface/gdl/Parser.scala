package epfl.algo.scarface.gdl

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
	
import java.io.InputStream
	
import epfl.algo.scarface.gdl.Gdl._

/**
 * Parser for Game Description Language
 */
class Parser(private val input_ : InputStream) extends StandardTokenParsers {
	
	import scala.util.parsing.input._
	import scala.util.parsing.syntax._
	
	def this(_input : String) = this(new java.io.ByteArrayInputStream(_input.getBytes))
	
	lexical.delimiters ++= List("(", ")", "<=", "?", ".")
	lexical.reserved ++= List(".")
    
    private val game_ : GdlList = parse()
    
    def game() : GdlList = game_
    
    private implicit def nameToSymbol(_name : String) : Symbol = Symbol(_name)
    
    private def GDL : Parser[GdlList] = (
        rep(TermList) ^^ { case l => GdlList(l) }
        | failure("Illegal relation!")
    )
    
    private def TermList : Parser[GdlList] = (
        "(" ~> rep(Term) <~ ")" ^^ { case l => GdlList(l) }
        | failure("Illegal relation!")
    )
    
    private def Term : Parser[GdlTerm] = (
        Atom
        | Variable
        | TermList
        | failure("Illegal atom!")
    )
    
    private def Atom : Parser[GdlAtom] = (
    	ident ~ "." ~ numericLit ^^ { case n1 ~ n2 ~ n3 => GdlAtom((n1 + n2 + n3) toLowerCase) }
        | ident ^^ { case name => GdlAtom(name toLowerCase) }
        | numericLit ^^ { case n => GdlAtom(n toString) }
        | "<=" ^^^ GdlAtom("<=")
        | failure("Illegal atom!")
    )
    
    private def Variable : Parser[GdlVar] = (
        "?" ~> ident ^^ { case name => GdlVar(name toUpperCase) }
        | failure("Illegal variable!")
    )
    
    private def parse() : GdlList = {
		val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(input_)))
        phrase(GDL)(tokens) match {
            case Success(trees, _) =>
                trees
            case e =>
                throw new RuntimeException(e.toString)
        }    
	}

}