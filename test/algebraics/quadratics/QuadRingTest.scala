package algebraics.quadratics

import algebraics.UnsupportedNumberDomainException

import org.junit.Test
import org.junit.Assert._

class QuadRingTest {

  @Test def testApply(): Unit = {
    var expected: QuadRing = new ImagQuadRing(-7)
    var actual: QuadRing = QuadRing(-7)
    assertEquals(expected, actual)
    expected = new RealQuadRing(13)
    actual = QuadRing(13)
    try {
      actual = QuadRing(1)
      val failMsg = "Should not have been able to create " + actual.toString + " by using apply(1)"
      fail(failMsg)
    } catch {
      case unde: UnsupportedNumberDomainException => println("Trying to use 1 for apply correctly triggered UnsupportedNumberDomainException")
        println("\"" + unde.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is wrong exception to throw for trying to 1 for apply"
        fail(failMsg)
    }
  }

}
