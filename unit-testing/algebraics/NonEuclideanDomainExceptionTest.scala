package algebraics

import algebraics.quadratics.ImagQuadInt
import algebraics.quadratics.ImagQuadRing
import algebraics.quadratics.QuadInt
//import algebraics.quadratics.RealQuadInt
//import algebraics.quadratics.RealQuadRing
import calculators.NumberTheoreticFunctionsCalculator.normWrap

import org.junit.Test
import org.junit.Assert._

class NonEuclideanDomainExceptionTest {
  private val ringZi5 = new ImagQuadRing(-5)
//  private val ringOQi19 = new ImagQuadRing(-19)
//  private val ringZ14 = new RealQuadRing(14)
//  private val ringOQ69 = new RealQuadRing(69)
  private var iqia: QuadInt = new ImagQuadInt(2, 0, ringZi5)
  private var iqib: QuadInt = new ImagQuadInt(1, 1, ringZi5)
  private val nonEuclExc6 = new NonEuclideanDomainException("gcd(2, 1 + sqrt(-5)) example", iqia, iqib, normWrap)
  iqia = new ImagQuadInt(29, 0, ringZi5)
  iqib = new ImagQuadInt(-7, 5, ringZi5)
  private val nonEuclExc29 = new NonEuclideanDomainException("gcd(29,-7 + 5sqrt(-5)) example", iqia, iqib, normWrap)
//  iqia = new ImagQuadInt(0, 11, ringZi5)
//  iqib = new ImagQuadInt(0, 13, ringZi5)
//  private val nonEuclExc143A = new NonEuclideanDomainException("gcd(11sqrt(-5), 13sqrt(-5)) example", iqia, iqib, normWrap)
//  iqib = new ImagQuadInt(13, 0, ringZi5)
//  private val nonEuclExc143B = new NonEuclideanDomainException("gcd(11sqrt(-5), 13) example", iqia, iqib, normWrap)

  @Test def testTryEuclideanGCDAnyway(): Unit = {
    println("")
    var actual = nonEuclExc6.tryEuclideanGCDAnyway
    var assertionMessage = "Attempted Re(gcd(" + nonEuclExc6.causingA.toUnicodeString + ", " + nonEuclExc6.causingB.toUnicodeString + ")) should be negative"
    assertTrue(assertionMessage, actual.asInstanceOf[QuadInt].regPart < 0)
    var expected: QuadInt = new ImagQuadInt(3, 2, ringZi5)
    actual = nonEuclExc29.tryEuclideanGCDAnyway
    assertionMessage = "gcd(" + nonEuclExc29.causingA.toUnicodeString + ", " + nonEuclExc29.causingB.toUnicodeString + ") should be " + expected.toUnicodeString
    assertEquals(assertionMessage, expected, actual)
  }

}
