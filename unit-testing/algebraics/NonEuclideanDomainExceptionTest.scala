package algebraics

import algebraics.quadratics.ImagQuadInt
import algebraics.quadratics.ImagQuadRing
import algebraics.quadratics.QuadInt
import algebraics.quadratics.RealQuadInt
import algebraics.quadratics.RealQuadRing
import calculators.NumberTheoreticFunctionsCalculator.normWrap

import org.junit.Test
import org.junit.Assert._

class NonEuclideanDomainExceptionTest {
  private val ringZi5 = new ImagQuadRing(-5)
  private val ringOQi19 = new ImagQuadRing(-19)
  private val ringZ14 = new RealQuadRing(14)
  private val ringOQ69 = new RealQuadRing(69)
  private var quadIntA: QuadInt = new ImagQuadInt(2, 0, ringZi5)
  private var quadIntB: QuadInt = new ImagQuadInt(1, 1, ringZi5)
  private val nonEuclExc6 = new NonEuclideanDomainException("gcd(2, 1 + sqrt(-5)) example", quadIntA, quadIntB, normWrap)
  quadIntA = new ImagQuadInt(29, 0, ringZi5)
  quadIntB = new ImagQuadInt(-7, 5, ringZi5)
  private val nonEuclExc29 = new NonEuclideanDomainException("gcd(29,-7 + 5sqrt(-5)) example", quadIntA, quadIntB, normWrap)
  quadIntA = new ImagQuadInt(0, 11, ringZi5)
  quadIntB = new ImagQuadInt(0, 13, ringZi5)
  private val nonEuclExc143A = new NonEuclideanDomainException("gcd(11sqrt(-5), 13sqrt(-5)) example", quadIntA, quadIntB, normWrap)
  quadIntB = new ImagQuadInt(13, 0, ringZi5)
  private val nonEuclExc143B = new NonEuclideanDomainException("gcd(11sqrt(-5), 13) example", quadIntA, quadIntB, normWrap)
  quadIntA = new ImagQuadInt(10, 0, ringOQi19)
  quadIntB = new ImagQuadInt(3, 1, ringOQi19, 2)
  private val nonEuclExc700 = new NonEuclideanDomainException("gcd(10, 3/2 + sqrt(-19)/2) example", quadIntA, quadIntB, normWrap)
  quadIntA = new RealQuadInt(2, 0, ringZ14)
  quadIntB = new RealQuadInt(1, 1, ringZ14)
  private val nonEuclExc13 = new NonEuclideanDomainException("gcd(2, 1 + sqrt(14))", quadIntA, quadIntB, normWrap)
  quadIntA = new RealQuadInt(0, 39, ringZ14)
  quadIntB = new RealQuadInt(-40, 12, ringZ14)
  private val nonEuclExc39 = new NonEuclideanDomainException("gcd(39sqrt(14), -40 + 12sqrt(14))", quadIntA, quadIntB, normWrap)
  quadIntA = new RealQuadInt(18, 2, ringOQ69)
  quadIntB = new RealQuadInt(23, 3, ringOQ69, 2)
  private val nonEuclExc48 = new NonEuclideanDomainException("gcd(18 + 2sqrt(69), 23/2 + 3sqrt(69)/2)", quadIntA, quadIntB, normWrap)

  @Test def testTryEuclideanGCDAnyway(): Unit = {
    println("tryEuclideanGCDAnyway")
    var actual = nonEuclExc6.tryEuclideanGCDAnyway
    var assertionMessage = "Attempted Re(gcd(" + nonEuclExc6.causingA.toUnicodeString + ", " + nonEuclExc6.causingB.toUnicodeString + ")) should be negative"
    assertTrue(assertionMessage, actual.asInstanceOf[QuadInt].regPart < 0)
    var expected: QuadInt = new ImagQuadInt(3, 2, ringZi5)
    actual = nonEuclExc29.tryEuclideanGCDAnyway
    assertionMessage = "gcd(" + nonEuclExc29.causingA.toUnicodeString + ", " + nonEuclExc29.causingB.toUnicodeString + ") should be " + expected.toUnicodeString
    assertEquals(assertionMessage, expected, actual)
    expected = new ImagQuadInt(0, 1, ringZi5)
    actual = nonEuclExc143A.tryEuclideanGCDAnyway
    assertionMessage = "gcd(" + nonEuclExc143A.causingA.toUnicodeString + ", " + nonEuclExc143A.causingB.toUnicodeString + ") should be " + expected.toUnicodeString
    assertEquals(assertionMessage, expected, actual)
    actual = nonEuclExc143B.tryEuclideanGCDAnyway
    // I FORGOT WHAT 143B IS SUPPOSED TO ASSERT
    actual = nonEuclExc700.tryEuclideanGCDAnyway
    assertionMessage = "Attempted Re(gcd(" + nonEuclExc700.causingA.toUnicodeString + ", " + nonEuclExc700.causingB.toUnicodeString + ")) should be negative"
    assertTrue(assertionMessage, actual.asInstanceOf[QuadInt].regPart < 0)
  }

}
