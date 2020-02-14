package calculators

import algebraics.quadratics.{ImagQuadInt, ImagQuadRing, RealQuadInt, RealQuadRing}

import org.junit.Test
import org.junit.Assert._

class EuclideanGCDProcSeqTest {
  private val ringZi5 = new ImagQuadRing(-5)
  private val ringZ2 = new RealQuadRing(2)

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

}
