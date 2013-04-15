package epfl.algo.scarface.proof

/**
 * An <code>UCTPlayerFactory</code> represents a <code>UCTPlayer</code> factory object.
 */
class GenericLogicFactory[T <: AbstractLogic](private val logicClassName : String)(private implicit val logicPath : String) extends AbstractLogicFactory {

	import epfl.algo.scarface.gdl.GameInfo
	import epfl.algo.scarface.proof.Expr._
	import epfl.algo.scarface.util.Reflection._
	
	private val logicClassPath = if(logicClassName.contains(".")) logicClassName else logicPath + "." + logicClassName
	
	// Specified by AbstractLogicFactory superclass
	override def logicNew(_gameInfo : GameInfo) : TuPrologWrapper = {
		
		implicit def classLoader : ClassLoader = this.clazz.getClassLoader
		
    	val state : State = new State(_gameInfo.staticFacts)
    	val rules : List[Rule] = _gameInfo.rulesList
    	
		new TuPrologWrapper(state, rules)
		
	}
  	
}