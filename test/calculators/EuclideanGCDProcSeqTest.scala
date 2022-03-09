package calculators

import algebraics.quadratics.{ImagQuadInt, ImagQuadRing, RealQuadInt, RealQuadRing}

import org.junit.Test
import org.junit.Assert._

class EuclideanGCDProcSeqTest {
  private val ringOQi19 = new ImagQuadRing(-19)
  private val ringZi5 = new ImagQuadRing(-5)
  private val ringZ2 = new RealQuadRing(2)
  private val ringZ14 = new RealQuadRing(14)
  private val ringOQ69 = new RealQuadRing(69)

  @Test def testNonEuclideanButUFDImagDomain(): Unit = {
    val a = new ImagQuadInt(10, 0, ringOQi19)
    val b = new ImagQuadInt(3, 1, ringOQi19)
    val euclSeq = new EuclideanGCDProcSeq(a, b)
    val expected = new ImagQuadInt(-1, 0, ringOQi19)
    val actual = euclSeq.result
    assertEquals(expected, actual)
    val assertionMessage = "Trying to calculate gcd(10, 3/2 + sqrt(-19)/2) should have caused NonEuclideanDomainException"
    assertNotNull(assertionMessage, euclSeq.problem)
    println(euclSeq.problem.getMessage)
  }

  @Test def testNonEuclideanDomain(): Unit = {
    val a = new ImagQuadInt(1, 1, ringZi5)
    val b = new ImagQuadInt(2, 0, ringZi5)
    val euclSeq = new EuclideanGCDProcSeq(a, b)
    val expected = new ImagQuadInt(-1, 0, ringZi5)
    val actual = euclSeq.result
    assertEquals(expected, actual)
    val assertionMessage = "Trying to calculate gcd(2, 1 + sqrt(-5) should have caused NonEuclideanDomainException"
    assertNotNull(assertionMessage, euclSeq.problem)
    println(euclSeq.problem.getMessage)
  }

  @Test def testEuclideanDomain(): Unit = {
    val a = new RealQuadInt(2, 1, ringZ2)
    val b = new RealQuadInt(2, 0, ringZ2)
    val euclSeq = new EuclideanGCDProcSeq(a, b)
    val expected = new RealQuadInt(0, 1, ringZ2)
    val actual = euclSeq.result
    assertEquals(expected, actual)
    val expectedList = List(a, b, expected, new RealQuadInt(0, 0, ringZ2))
    val actualList = euclSeq.euclideanSeq
    assertEquals(expectedList, actualList)
  }

  @Test def testNonNormEuclideanButUFDRealDomain(): Unit = {
    val a = new RealQuadInt(1, 1, ringZ14)
    val b = new RealQuadInt(2, 0, ringZ14)
    val euclSeq = new EuclideanGCDProcSeq(a, b)
    val expected = new RealQuadInt(-1, 0, ringZ14)
    val actual = euclSeq.result
    assertEquals(expected, actual)
    val assertionMessage = "Trying to calculate gcd(1 + sqrt(14), 2) should have caused NonEuclideanDomainException"
    assertNotNull(assertionMessage, euclSeq.problem)
    println(euclSeq.problem.getMessage)
  }

  @Test def testEuclideanButNotForNorm(): Unit = {
    val a = new RealQuadInt(18, 2, ringOQ69)
    val b = new RealQuadInt(23, 3, ringOQ69, 2)
    val euclSeq = new EuclideanGCDProcSeq(a, b)
    val expected = new RealQuadInt(-1, 0, ringOQ69)
    val actual = euclSeq.result
    assertEquals(expected, actual)
    val assertionMessage = "Trying to calculate gcd(18 + 2sqrt(69), 23/2 + 3sqrt(69)/2) with absolute value of the norm should have caused NonEuclideanDomainException"
    assertNotNull(assertionMessage, euclSeq.problem)
    println(euclSeq.problem.getMessage)
  }

  @Test def testEuclideanForAdjNorm(): Unit = {
    val a = new RealQuadInt(18, 2, ringOQ69)
    val b = new RealQuadInt(23, 3, ringOQ69, 2)
    val euclSeq = new EuclideanGCDProcSeq(a, b, NumberTheoreticFunctionsCalculatorTest.adj23Norm)
    val expected = new RealQuadInt(25, -3, ringOQ69, 2)
    val actual = euclSeq.result
    assertEquals(expected, actual)
  }

  @Test def testEuclideanButFunctionInvalid(): Unit = {
    val a = new RealQuadInt(5, 3, ringZ14)
    val b = new RealQuadInt(7, 2, ringZ14)
    try {
      val euclSeq = new EuclideanGCDProcSeq(a, b, NumberTheoreticFunctionsCalculatorTest.invalidFunctionF)
      println("\"" + euclSeq.problem.getMessage + "\"")
      val failMsg = "Trying to use invalid function for Euclidean GCD should have caused an exception, not given result " + euclSeq.result.toString
      fail(failMsg)
    } catch {
      case iae: IllegalArgumentException => println("Trying to use invalid function for Euclidean GCD correctly triggered IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is the wrong exception to throw for trying to use invalid function for Euclidean GCD"
        fail(failMsg)
    }
  }

  @Test def testEuclidean1024(): Unit = {
    val a = new RealQuadInt(1024, 0, ringZ2)
    val b = new RealQuadInt(6, 3, ringZ2)
    val euclSeq = new EuclideanGCDProcSeq(a, b)
    val expected = new RealQuadInt(2, 1, ringZ2)
    val actual = euclSeq.result
    assertEquals(expected, actual)
    assertNull(euclSeq.problem)
  }

}
