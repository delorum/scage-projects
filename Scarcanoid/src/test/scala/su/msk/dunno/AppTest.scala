package su.msk.dunno;

import junit.framework._;
import Assert._
import scar.Scaranoid

object AppTest {
    def suite: Test = {
        val suite = new TestSuite(classOf[AppTest]);
        suite
    }

    def main(args : Array[String]) {
        junit.textui.TestRunner.run(suite);
    }
}

/**
 * Unit test for simple App.
 */
class AppTest extends TestCase("app") {

    /**
     * Rigourous Tests :-)
     */
    Scaranoid.run
    def testOK() = assertTrue(true);
    

}
