package epfl.algo.scarface.player

/**
 * An <code>GenericPlayerFactory</code> represents a <code>AbstractPlayer</code> factory object.
 * 
 * @type T - the requested single player type
 * @type U - the request multi player type
 */
class GenericPlayerFactory[T <: AbstractPlayer, U <: AbstractPlayer](private val singlePlayerClassName : String, private val multiPlayerClassName : String)(private implicit val playerPath : String) extends AbstractPlayerFactory {
	import epfl.algo.scarface.gdl.{Parser, GameInfo}
    import epfl.algo.scarface.gdl.Gdl._
    
    import epfl.algo.scarface.proof.{GenericLogicFactory, SimpleReasoner, State, TuPrologWrapper}

	import epfl.algo.scarface.util.Reflection._
	
	private val singlePlayerClassPath = if(singlePlayerClassName.contains(".")) singlePlayerClassName else playerPath + "." + singlePlayerClassName
	private val multiPlayerClassPath = if(multiPlayerClassName.contains(".")) multiPlayerClassName else playerPath + "." + multiPlayerClassName
	
	private implicit def classLoader : ClassLoader = this.clazz.getClassLoader
	
	// Specified by AbstractPlayerFactory superclass
	override def playerNew(_gameId : String, _role : GdlAtom, _gameDescription : GdlList, _startClock : Int, _playClock : Int) : AbstractPlayer = {
		val gameInfo : GameInfo = new GameInfo(_gameDescription)
		
		val logicFactory = new GenericLogicFactory[TuPrologWrapper]("TuPrologWrapper")("epfl.algo.scarface.proof")
    	
	    val reasoner = new SimpleReasoner(logicFactory.logicNew(gameInfo))
		
		if(gameInfo.roles.size == 1)
			New(singlePlayerClassPath)(_gameId, reasoner, _role, gameInfo, _startClock, _playClock)
		else
			New(multiPlayerClassPath)(_gameId, reasoner, _role, gameInfo, _startClock, _playClock)
	}
  	
}