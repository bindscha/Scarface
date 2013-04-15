package epfl.algo.scarface.util

object HTTPTools {

  val SEPARATOR = "\n"
  
  val REPLY_HEADER = 	"HTTP/1.0 200 OK" + SEPARATOR +
            			"Content-type: text/acl" + SEPARATOR +
            			"Content-length: "
  
  val READY = "READY"
  
  def explanation(expl : String) : String = 
    "(EXPLANATION \"" + escape(expl) + "\")"
  
  def taunt(taunt : String) : String = 
    "(TAUNT \"" + escape(taunt) + "\")"
  
  def escape(str : String) : String = 
    (str map ((c : Char) => (if(c == '\"' || c == '\\') "\\" else "") + c)).mkString
}
