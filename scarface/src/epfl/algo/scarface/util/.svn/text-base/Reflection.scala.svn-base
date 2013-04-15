package epfl.algo.scarface.util

object Reflection {
	
	def New[T <: AnyRef](className : String)(args : WithType*)(implicit classLoader : ClassLoader): T  = {
	    val clazz: Class[T] = className
	    val argTypes = args map { _.clazz } toArray
	    val candidates = clazz.getConstructors filter { cons => matchingTypes(cons.getParameterTypes, argTypes) }
	    require(candidates.length == 1, "Argument runtime types must select exactly one constructor")
	    val params = args map { _.value }
	    candidates.first.newInstance(params: _*).asInstanceOf[T]
	}

	
	private implicit def className2Class[T <: AnyRef](className : String)(implicit classLoader : ClassLoader) : Class[T] =
		Class.forName(className, true, classLoader).asInstanceOf[Class[T]]
	
	private def matchingTypes(declared: Array[Class[_]], actual: Array[Class[_]]): Boolean = {
		declared.length == actual.length && (
		(declared zip actual) forall {
        	case (declared, actual) => declared.isAssignableFrom(actual)
		})
	}
	
	sealed abstract class WithType {
		val clazz : Class[_]
		val value : AnyRef
	}
  
  	case class ValWithType(anyVal: AnyVal, clazz: Class[_]) extends WithType {
  		lazy val value = toAnyRef(anyVal)
  	}
  
  	case class RefWithType(anyRef: AnyRef, clazz: Class[_]) extends WithType {
  		val value = anyRef
  	}
  	
  	implicit def refWithType[T <: AnyRef](x : T) : RefWithType = RefWithType(x, x.getClass)
  	implicit def valWithType[T <: AnyVal](x : T) : ValWithType = ValWithType(x, getType(x))
  	
  	private def getType(x: AnyVal): Class[_] = x match {
	    case _: Byte => java.lang.Byte.TYPE
	    case _: Short => java.lang.Short.TYPE
	    case _: Int => java.lang.Integer.TYPE
	    case _: Long => java.lang.Long.TYPE
	    case _: Float => java.lang.Float.TYPE
	    case _: Double => java.lang.Double.TYPE
	    case _: Char => java.lang.Character.TYPE
	    case _: Boolean => java.lang.Boolean.TYPE
	    case _: Unit => java.lang.Void.TYPE
  	}

	private def toAnyRef(x: AnyVal): AnyRef = x match {
	    case x: Byte => Byte.box(x)
	    case x: Short => Short.box(x)
	    case x: Int => Int.box(x)
	    case x: Long => Long.box(x)
	    case x: Float => Float.box(x)
	    case x: Double => Double.box(x)
	    case x: Char => Char.box(x)
	    case x: Boolean => Boolean.box(x)
	    case x: Unit => ()
	}
	
}