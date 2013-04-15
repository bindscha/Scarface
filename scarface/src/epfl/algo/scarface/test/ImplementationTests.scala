package epfl.algo.scarface.test

import junit.framework._

object ImplementationTests extends TestSuite {

  addTestSuite(classOf[LogicTest2])
  
  def suite(): Test = {
		return ImplementationTests
  }
  
}
