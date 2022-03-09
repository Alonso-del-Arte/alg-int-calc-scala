package viewers

import algebraics.quadratics.ImagQuadRing
// TODO: Switch over to JUnit 5
import org.junit.Test
import org.junit.Assert._

class ImagQuadRingDisplayTest {
  private val ring = new ImagQuadRing(-1)
  private val ird = new ImagQuadRingDisplay(ring)

  @Test def testSetPixelsPerUnitInterval(): Unit = {
    println("setPixelsPerUnitInterval")
    try {
      ird.setPixelsPerUnitInterval(RingDisplay.MINIMUM_PIXELS_PER_UNIT_INTERVAL - 1)
      val msg = "Trying to set pixels per unit interval below " +
        RingDisplay.MINIMUM_PIXELS_PER_UNIT_INTERVAL.toString +
        " should have caused an exception"
      fail(msg)
    } catch {
      case iae: IllegalArgumentException =>
        println("Trying to set pixels per unit interval below "
          + RingDisplay.MINIMUM_PIXELS_PER_UNIT_INTERVAL.toString
          + " correctly triggered IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
      case e: Exception => val msg = e.getClass.getName +
        " is wrong exception to throw for trying to set pixels per unit interval below " +
        RingDisplay.MINIMUM_PIXELS_PER_UNIT_INTERVAL.toString
        fail(msg)
    }
  }

}
