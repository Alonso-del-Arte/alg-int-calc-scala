package algebraics.quadratics

import org.junit.Test
import org.junit.Assert._

class ImagQuadIntLineTest {

  private val ringGaussian = new ImagQuadRing(-1)
  private val ringZi2 = new ImagQuadRing(-2)
  private val ringEisenstein = new ImagQuadRing(-3)
  private val ringOQi7 = new ImagQuadRing(-7)

  private val gaussianStart = new ImagQuadInt(1, 1, ringGaussian)
  private val gaussianEnd = new ImagQuadInt(5, 5, ringGaussian)
  private val zi2Start = new ImagQuadInt(-2, 0, ringZi2)
  private val zi2End = new ImagQuadInt(-11, 3, ringZi2)
  private val eisensteinStart = new ImagQuadInt(-7, 0, ringEisenstein)
  private val eisensteinEnd = new ImagQuadInt(-5, -2, ringEisenstein)
  private val oqi7Start = new ImagQuadInt(-1, -3, ringOQi7, 2)
  private val oqi7End = new ImagQuadInt(-11, -3, ringOQi7)

  private val gaussianLine = new ImagQuadIntLine(gaussianStart, gaussianEnd)
  private val zi2Line = new ImagQuadIntLine(zi2Start, zi2End)
  private val eisensteinLine = new ImagQuadIntLine(eisensteinStart, eisensteinEnd)
  private val oqi7Line = new ImagQuadIntLine(oqi7Start, oqi7End)

  @Test def testLength(): Unit = {
    println("length")
    var expected = 5
    var actual = gaussianLine.length
    assertEquals(expected, actual)
    actual = eisensteinLine.length
    assertEquals(expected, actual)
    expected = 4
    actual = zi2Line.length
    assertEquals(expected, actual)
    actual = oqi7Line.length
    assertEquals(expected, actual)
    val inferredStep = ImagQuadInt.inferStep(zi2Start, zi2End)
    val byStep = (inferredStep * 2).asInstanceOf[ImagQuadInt]
    val byEnd = (zi2End + inferredStep).asInstanceOf[ImagQuadInt]
    val zi2ByLine = new ImagQuadIntLine(zi2Start, byEnd, byStep)
    expected = 3
    actual = zi2ByLine.length
    assertEquals(expected, actual)
  }

  @Test def testApply(): Unit = {
    println("apply")
    var curr: QuadInt = gaussianStart
    var step = new ImagQuadInt(1, 1, ringGaussian)
    for (i <- 0 to 4) {
      val expected = curr
      val actual = gaussianLine.apply(i)
      assertEquals(expected, actual)
      curr = curr + step
    }
    curr = zi2Start
    step = new ImagQuadInt(-3, 1, ringZi2)
    for (j <- 0 to 3) {
      val expected = curr
      val actual = zi2Line.apply(j)
      assertEquals(expected, actual)
      curr = curr + step
    }
    curr = eisensteinStart
    step = new ImagQuadInt(1, -1, ringEisenstein, 2)
    for (k <- 0 to 4) {
      val expected = curr
      val actual = eisensteinLine.apply(k)
      assertEquals(expected, actual)
      curr = curr + step
    }
    curr = oqi7Start
    step = new ImagQuadInt(-7, -1, ringOQi7, 2)
    for (l <- 0 to 3) {
      val expected = curr
      val actual = oqi7Line.apply(l)
      assertEquals(expected, actual)
      curr = curr + step
    }
  }

  @Test def testApplyOutOfBounds(): Unit = {
    try {
      val result = gaussianLine.apply(-1)
      val failMsg = "Trying to use negative index should have caused an exception, not given result " + result.toString
      fail(failMsg)
    } catch {
      case ioobe: IndexOutOfBoundsException => println("Trying to use negative index correctly caused IndexOutOfBoundsException")
        println("\"" + ioobe.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is the wrong exception for trying to use negative index"
        fail(failMsg)
    }
    try {
      val result = eisensteinLine.apply(500)
      val failMsg = "Trying to use index beyond last index should have caused an exception, not given result " + result.toString
      fail(failMsg)
    } catch {
      case ioobe: IndexOutOfBoundsException => println("Trying to use index beyond last index correctly caused IndexOutOfBoundsException")
        println("\"" + ioobe.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is the wrong exception for trying to use index beyond last index"
        fail(failMsg)
    }
  }

}
