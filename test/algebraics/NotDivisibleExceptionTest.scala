package algebraics

import algebraics.quadratics.{ImagQuadInt, ImagQuadRing, QuadInt, RealQuadInt, RealQuadRing}
import fractions.Fraction

import org.junit.Test
import org.junit.Assert._

class NotDivisibleExceptionTest {

  private val ringGaussian = new ImagQuadRing(-1)
  private val ringEisenstein = new ImagQuadRing(-3)
  private val ringZ2 = new RealQuadRing(2)
  private val ringZPhi = new RealQuadRing(5)

  private val gaussianDividend = new ImagQuadInt(5, 1, ringGaussian)
  private val gaussianDivisor = new ImagQuadInt(3, 1, ringGaussian)
  private val eisensteinDividend = new ImagQuadInt(61, 0, ringEisenstein)
  private val eisensteinDivisor = new ImagQuadInt(1, 9, ringEisenstein)
  private val z2Dividend = new RealQuadInt(3, -8, ringZ2)
  private val z2Divisor = new RealQuadInt(7, 0, ringZ2)
  private val zPhiDividend = new RealQuadInt(0, 1, ringZPhi)
  private val zPhiDivisor = new RealQuadInt(15, 3, ringZPhi, 2)

  private val gaussianDivFractRe = new Fraction(8, 5)
  private val gaussianDivFractIm = new Fraction(-1, 5)
  private val eisensteinDivFractRe = new Fraction(1, 4)
  private val eisensteinDivFractIm = new Fraction(-9, 4)
  private val z2DivFractReg = new Fraction(3, 7)
  private val z2DivFractSurd = new Fraction(-8, 7)
  private val zPhiDivFractReg = new Fraction(1, 6)
  private val zPhiDivFractSurd = new Fraction(-1, 30)

  private val notDivGaussian = new NotDivisibleException("Z[i] example", gaussianDividend, gaussianDivisor, Array(gaussianDivFractRe, gaussianDivFractIm))
  private val notDivEisenstein = new NotDivisibleException("Z[omega] example", eisensteinDividend, eisensteinDivisor, Array(eisensteinDivFractRe, eisensteinDivFractIm))
  private val notDivZ2 = new NotDivisibleException("Z[sqrt(2)] example", z2Dividend, z2Divisor, Array(z2DivFractReg, z2DivFractSurd))
  private val notDivZPhi = new NotDivisibleException("Z[phi] example", zPhiDividend, zPhiDivisor, Array(zPhiDivFractReg, zPhiDivFractSurd))

  private val testDelta = 0.00000001

  @Test def testGetNumericRealPart(): Unit = {
    println("getNumericRealPart")
    assertEquals(gaussianDivFractRe.numericApproximation, notDivGaussian.getNumericRealPart, testDelta)
    assertEquals(eisensteinDivFractRe.numericApproximation, notDivEisenstein.getNumericRealPart, testDelta)
    var expRe = z2DivFractReg.numericApproximation + z2DivFractSurd.numericApproximation * Math.sqrt(2)
    assertEquals(expRe, notDivZ2.getNumericRealPart, testDelta)
    expRe = zPhiDivFractReg.numericApproximation + zPhiDivFractSurd.numericApproximation * ringZPhi.getRadSqrt
    assertEquals(expRe, notDivZPhi.getNumericRealPart, testDelta)
  }

  @Test def testGetNumericImagPart(): Unit = {
    println("getNumericImagPart")
    assertEquals(gaussianDivFractIm.numericApproximation, notDivGaussian.getNumericImagPart, testDelta)
    assertEquals(eisensteinDivFractIm.numericApproximation * Math.sqrt(3), notDivEisenstein.getNumericImagPart, testDelta)
    assertEquals(0.0, notDivZ2.getNumericImagPart, testDelta)
    assertEquals(0.0, notDivZPhi.getNumericImagPart, testDelta)
  }

  @Test def testGetAbs(): Unit = {
    println("getAbs")
    var expected = Math.sqrt((gaussianDivFractRe * gaussianDivFractRe + gaussianDivFractIm * gaussianDivFractIm).numericApproximation)
    var actual = notDivGaussian.getAbs
    assertEquals(expected, actual, testDelta)
    expected = Math.sqrt((eisensteinDivFractRe * eisensteinDivFractRe + 3 * eisensteinDivFractIm * eisensteinDivFractIm).numericApproximation)
    actual = notDivEisenstein.getAbs
    assertEquals(expected, actual, testDelta)
    expected = Math.abs(z2DivFractReg.numericApproximation + z2DivFractSurd.numericApproximation * Math.sqrt(2))
    actual = notDivZ2.getAbs
    assertEquals(expected, actual, testDelta)
    expected = zPhiDivFractReg.numericApproximation + zPhiDivFractSurd.numericApproximation * Math.sqrt(5)
    actual = notDivZPhi.getAbs
    assertEquals(expected, actual, testDelta)
  }

  @Test def testGetBoundingIntegers(): Unit = {
    println("getBoundingIntegers")
    val one = new ImagQuadInt(1, 0, ringGaussian)
    val two = new ImagQuadInt(2, 0, ringGaussian)
    val twoMinusImagUnit = new ImagQuadInt(2, -1, ringGaussian)
    val oneMinusImagUnit = new ImagQuadInt(1, -1, ringGaussian)
    var expected = List(one, oneMinusImagUnit, two, twoMinusImagUnit)
    var actual = notDivGaussian.getBoundingIntegers.sortBy(_.norm)
    assertEquals(expected, actual)
    val negTwoSqrtNeg3 = new ImagQuadInt(0, -2, ringEisenstein)
    val negThreeNegFiveOmega = new ImagQuadInt(-1, -5, ringEisenstein, 2)
    val negTwoNegFiveOmega = new ImagQuadInt(1, -5, ringEisenstein, 2)
    val negThreeSqrtNeg3 = new ImagQuadInt(0, -3, ringEisenstein)
    expected = List(negTwoSqrtNeg3, negThreeNegFiveOmega, negTwoNegFiveOmega, negThreeSqrtNeg3)
    actual = notDivEisenstein.getBoundingIntegers.sortBy(_.norm)
    assertEquals(expected, actual)
  }

  @Test def testRoundTowardsZero(): Unit = {
    println("roundTowardsZero")
    var expected: QuadInt = new ImagQuadInt(1, 0, ringGaussian)
    var actual = notDivGaussian.roundTowardsZero
    assertEquals(expected, actual)
    expected = new ImagQuadInt(0, -2, ringEisenstein)
    actual = notDivEisenstein.roundTowardsZero
    assertEquals(expected, actual)
//    expected = new RealQuadInt(1, 0, ringZ2)
//    actual = notDivZ2.roundTowardsZero
//    assertEquals(expected, actual)
//    expected = new RealQuadInt(0, 0, ringZPhi)
//    actual = notDivZPhi.roundTowardsZero
//    assertEquals(expected, actual)
  }

  @Test def testRoundAwayFromZero(): Unit = {
    println("roundAwayFromZero")
    var expected: QuadInt = new ImagQuadInt(2, -1, ringGaussian)
    var actual = notDivGaussian.roundAwayFromZero
    assertEquals(expected, actual)
    expected = new ImagQuadInt(0, -3, ringEisenstein)
    actual = notDivEisenstein.roundAwayFromZero
    assertEquals(expected, actual)
//    expected = new RealQuadInt(1, 0, ringZ2)
//    actual = notDivZ2.roundAwayFromZero
//    assertEquals(expected, actual)
//    expected = new RealQuadInt(0, 0, ringZPhi)
//    actual = notDivZPhi.roundAwayFromZero
//    assertEquals(expected, actual)
  }

}
