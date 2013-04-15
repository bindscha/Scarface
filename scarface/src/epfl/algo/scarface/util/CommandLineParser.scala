package epfl.algo.scarface.util

object FlagStatus extends Enumeration {
	type FlagStatus = Value
	val ON, OFF = Value
}

class CommandLineParser extends Logging {

	import scala.collection.mutable.{Map, HashMap}
	
    import FlagStatus._
	
	private val flags_ : Map[String, FlagStatus] = new HashMap[String, FlagStatus]
	private val parameters_ : Map[String, Option[String]] = new HashMap[String, Option[String]]
	
    private var usage_ : String = ""
    
    def usageIs(_usage : String) : Unit = 
    	usage_ = _usage
    	
	def flagNew(_name : String) : Unit = {
    	if((flags_ contains _name) || (parameters_ contains _name))
    		throw new IllegalArgumentException("Flag " + _name + " was already registered!")
		flags_ += ((_name, OFF))
	}
    
    def parameterNew(_name : String) : Unit = {
    	if((flags_ contains _name) || (parameters_ contains _name))
    		throw new IllegalArgumentException("Parameter " + _name + " was already registered!")
    	parameters_ += ((_name, None))
    }
    
    def flagsNew(_flags : String*) : Unit = 
    	_flags foreach flagNew
    	
    def parametersNew(_parameters : String*) : Unit = 
    	_parameters foreach flagNew

    def flag(_name : String) : Option[FlagStatus] = 
    	flags_ get _name
    
	def parameter(_name : String) : Option[String] = 
		parameters_ get _name match {
    		case Some(parameter) => parameter
    		case None => None
    	}
	
	def parameterAsInt(_name : String) : Option[Int] =
		parameter(_name) match {
    		case Some(value) => Some(value.toInt)
    		case None => None
    	}

	def parse(_args : Array[String]) : Unit = 
		for(val arg : String <- _args) {
			val eqIndex = arg indexOf "="
			
			if(eqIndex == -1) {
				if(flags_ contains arg) {
					flags_ + ((arg, ON))
				} else {
					println("Unrecognized flag " + arg + "!")
					println(usage_)
					exit(-1)
				}
			} else {
				val (parameter, value) = (arg substring (0, eqIndex), arg substring (eqIndex + 1))
				if(parameters_ contains parameter) {
					parameters_ + ((parameter, value))
				} else {
					println("Unrecognized parameter " + arg + "!")
					println(usage_)
					exit(-1)
				}
			}
				
		}
	
}