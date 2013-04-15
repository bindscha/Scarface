package epfl.algo.scarface.gdl

import epfl.algo.scarface.util.Utils.Identifier

object % extends (Symbol => Identifier) {
	
	import scala.collection.mutable.{Map, HashMap}
	import scala.util.Random

	private val table_ : Map[Symbol, Identifier] = new HashMap[Symbol, Identifier]
	
	val random_ : Random = new Random()
	
	def apply(_symbol : Symbol) = 
		table_ getOrElseUpdate(_symbol, random_.nextLong)

}