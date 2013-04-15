package epfl.algo.scarface.proof

/**
 * An <code>AbstractLogicFactory</code> represents a basic logic factory object.
 */
trait AbstractLogicFactory {

  import epfl.algo.scarface.gdl.GameInfo
  
  /**
   * Creates and returns a new <code>AbstractLogic</code> with the provided arguments.
   */
  def logicNew(gameInfo : GameInfo) : AbstractLogic
  
}
