package epfl.algo.scarface

import epfl.algo.scarface.util.Logging

/**
 * Entry point for the Scarface application.
 */
object Main extends Logging {
  
  import epfl.algo.scarface.game.GameManager
  import epfl.algo.scarface.player._
  import epfl.algo.scarface.network.{ConnectionManager, RequestHandlerFactory}
  import epfl.algo.scarface.util.CommandLineParser
  import epfl.algo.scarface.util.FlagStatus.{ON, OFF}
  
  val DEFAULT_PORT = 4001
  
  /**
   * Main method.
   */
  def main(_args : Array[String]) : Unit = {
    
    println(" ############################################")
    println(" # Welcome to Scarface General Game Player. #")
    println(" ############################################")
    println();

    val commandLineParser : CommandLineParser = new CommandLineParser()
    commandLineParser flagNew "--daemon"
    commandLineParser parameterNew "--port"
    commandLineParser usageIs """usage: <--daemon> <--port='portNumber'>"""
    
    commandLineParser parse _args
    
    val daemonMode = 
    	if((commandLineParser flag "--daemon") == ON) 
    		true 
    	else 
    		false 
    
    val port: Int = 
	    commandLineParser parameterAsInt "--port" match {
	    	case Some(value) => value
	    	case None => DEFAULT_PORT
	    }
    
    if ( ! daemonMode ) {
        println(" ########################################")
        println(" # Press Enter to shut the player down. #")
        println(" ########################################")
        println()
    } else {
        println(" ############################################################")
        println(" # Send \"(KILL abc)\" on port " + port + " to shut the player down. #")
        println(" ############################################################")
        println()
    }
    
    debug(if(daemonMode) "Running in daemon mode" else "Running as an application")
    debug("Listening port is " + port)
    
    // Set player factory
    implicit val playerPath : String = "epfl.algo.scarface.player"
    GameManager playerFactoryIs /*new GenericPlayerFactory[RandomPlayer, RandomPlayer]("RandomPlayer", "RandomPlayer")*/new GenericPlayerFactory[EIDAPlayer, UCTPlayer]("EIDAPlayer", "UCTPlayer")
    
    // Start connection manager...
    try {
        val manager = new ConnectionManager(port, new RequestHandlerFactory())
        manager.start()
        
        if ( !daemonMode ) {
            // Wait for input to kill the program
            System.in.read
            manager.shutdown
        } else {
            manager.join()
        }
        
        println(" ##############################")
        println(" # Scarface player shut down! #")
        println(" ##############################")
        println()
        
        // All done.
        System.exit(0)
        
        }
        catch {
          case e => println("Exception was thrown during shutdown: "); e.printStackTrace()
        }
    
  }
  
}
