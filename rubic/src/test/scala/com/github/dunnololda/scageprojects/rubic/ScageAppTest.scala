package com.github.dunnololda.scageprojects.rubic

import junit.framework.Assert._
import junit.framework._

object ScageAppTest {
    def suite: Test = {
        val suite = new TestSuite(classOf[ScageAppTest])
        suite
    }

    def main(args : Array[String]) {
        junit.textui.TestRunner.run(suite)
    }
}

/**
 * Unit test for simple App.
 */
class ScageAppTest extends TestCase("app") {

    /**
     * Rigourous Tests :-)
     */
    def testOK() = {
      Rubic.main(Array[String]())
      assertTrue(true)
    }
    //def testKO() = assertTrue(false);
    

}
