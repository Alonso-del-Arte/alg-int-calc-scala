package algebraics

import algebraics.quadratics.ImagQuadInt
import algebraics.quadratics.ImagQuadRing
import algebraics.quadratics.QuadInt
import algebraics.quadratics.RealQuadInt
import algebraics.quadratics.RealQuadRing
import calculators.NumberTheoreticFunctionsCalculator.primeFactors

import org.junit.Test
import org.junit.Assert._

class NonUniqueFactorizationDomainExceptionTest {
  private val ringZi5 = new ImagQuadRing(-5)
  private val sqrti5 = new ImagQuadInt(0, 1, ringZi5)
  private val nufdeSqrti5 = new NonUniqueFactorizationDomainException("sqrti5", sqrti5)
  private val five = new ImagQuadInt(5, 0, ringZi5)
  private val nufde05 = new NonUniqueFactorizationDomainException("05", five)
  private val six = new ImagQuadInt(6, 0, ringZi5)
  private val nufde06 = new NonUniqueFactorizationDomainException("06", six)
  private val zi5pf41 = new ImagQuadInt(6, 1, ringZi5)
  private val nufde41pfZi5 = new NonUniqueFactorizationDomainException("factor of 41", zi5pf41)
  private val twoScoreAndOne = new ImagQuadInt(41, 0, ringZi5)
  private val nufde41 = new NonUniqueFactorizationDomainException("41", twoScoreAndOne)
  private val numNorm630 = new ImagQuadInt(25, 1, ringZi5)
  private val nufdeNorm630 = new NonUniqueFactorizationDomainException("norm 630", numNorm630)
  private val ringZ10 = new RealQuadRing(10)
  private val ten = new RealQuadInt(10, 0, ringZ10)
  private val nufde10 = new NonUniqueFactorizationDomainException("10", ten)
  private val seventySeven = new RealQuadInt(77, 0, ringZ10)
  private val nufde77 = new NonUniqueFactorizationDomainException("77", seventySeven)
  private val oneAndTwoScore = new RealQuadInt(41, 0, ringZ10)
  private val nufde41Z10 = new NonUniqueFactorizationDomainException("41", oneAndTwoScore)
  private val numNorm390 = new RealQuadInt(-40, -11, ringZ10)
  private val nufdeNorm390 = new NonUniqueFactorizationDomainException("norm 390", numNorm390)

  @Test def testTryToFactorizeAnyway(): Unit = {
    println("tryToFactorizeAnyway")
    var expected: Vector[QuadInt] = Vector[ImagQuadInt](sqrti5)
    var actual = nufdeSqrti5.tryToFactorizeAnyway
    assertEquals(expected, actual)
    var negOneZi5 = new ImagQuadInt(-1, 0, ringZi5)
    expected = Vector[ImagQuadInt](negOneZi5, sqrti5, sqrti5)
    actual = nufde05.tryToFactorizeAnyway
    assertEquals(expected, actual)
    actual = nufde06.tryToFactorizeAnyway
    val negOneZi5Slice = Vector[ImagQuadInt](negOneZi5, negOneZi5)
    var assertionMessage = "Factorization of 6 in Z[sqrt(-5)] should include -1 twice, consecutively"
    assertTrue(assertionMessage, actual.containsSlice(negOneZi5Slice))
    val one: QuadInt = new ImagQuadInt(1, 0, ringZi5)
    var productCheck: QuadInt = actual.foldLeft(one)(_ * _.asInstanceOf[QuadInt])
    assertEquals(six, productCheck)
  }

  @Test def testTryToFactorizeAnywayInSeveralImagRings(): Unit = {
    fail("Have not written test yet")
  }

  @Test def testTryToFactorizeAnywayInSeveralRealRings(): Unit = {
    fail("Have not written test yet")
  }

}
