package algebraics.quadratics

import algebraics.{AlgebraicDegreeOverflowException, NotDivisibleException}
import calculators.NumberTheoreticFunctionsCalculator

import org.junit.Test
import org.junit.Assert._

class ImagQuadIntTest {
  private val ringGaussian = new ImagQuadRing(-1)
  private val ringZi2 = new ImagQuadRing(-2)
  private val ringEisenstein = new ImagQuadRing(-3)
  private val ringOQi7 = new ImagQuadRing(-7)
  private val ringOQi19 = new ImagQuadRing(-19)
  private val ringZi21 = new ImagQuadRing(-21)
  private val ringZi37 = new ImagQuadRing(-37)
  private val testDiscr = NumberTheoreticFunctionsCalculator.randomSquarefreeNumber(8192) match {
    case 1 => -5
    case 3 => -6
    case n: Int => -n
  }
  private val ringRandom = new ImagQuadRing(testDiscr)

  @Test def testInferStep(): Unit = {
    println("inferStep")
    var startingPoint = new ImagQuadInt(0, 0, ringGaussian)
    var endingPoint = new ImagQuadInt(7, 7, ringGaussian)
    var expected = new ImagQuadInt(1, 1, ringGaussian)
    var actual = ImagQuadInt.inferStep(startingPoint, endingPoint)
    assertEquals(expected, actual)
    startingPoint = new ImagQuadInt(-2, 0, ringZi2)
    endingPoint = new ImagQuadInt(-11, 3, ringZi2)
    expected = new ImagQuadInt(-3, 1, ringZi2)
    actual = ImagQuadInt.inferStep(startingPoint, endingPoint)
    assertEquals(expected, actual)
  }

  @Test def testConstructor(): Unit = {
    try {
      val badQuadInt = new ImagQuadInt(3, 1, ringGaussian, 2)
      val failMsg = "Should not have been able to create " + badQuadInt.toString
      fail(failMsg)
    } catch {
      case iae: IllegalArgumentException => println("Trying to create 3/2 + sqrt(-1)/2 as quadratic integer correctly triggered IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is not the right exception for trying to create 3/2 + sqrt(-1)/2 as quadratic integer"
        fail(failMsg)
    }
    try {
      val badQuadInt = new ImagQuadInt(5, 2, ringEisenstein, 2)
      val failMsg = "Should not have been able to create " + badQuadInt.toString
      fail(failMsg)
    } catch {
      case iae: IllegalArgumentException => println("Trying to create 5/2 + sqrt(-3) as quadratic integer correctly triggered IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is not the right exception for trying to create 5/2 + sqrt(-3) as quadratic integer"
        fail(failMsg)
    }
    try {
      val badQuadInt = new ImagQuadInt(5, 3, ringEisenstein, 4)
      val failMsg = "Should not have been able to create " + badQuadInt.toString
      fail(failMsg)
    } catch {
      case iae: IllegalArgumentException => println("Trying to create 5/4 + 3sqrt(-3)/4 as quadratic integer correctly triggered IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is not the right exception for trying to create 5/4 + 3sqrt(-3)/4 as quadratic integer"
        fail(failMsg)
    }
  }

  @Test def testAlgebraicDegree(): Unit = {
    println("algebraicDegree")
    val zeroIQI = new ImagQuadInt(0, 0, ringRandom)
    assertEquals(0, zeroIQI.algebraicDegree)
    for (a <- -19 to 19 by 2) {
      val purelyRealInt = new ImagQuadInt(a, 0, ringGaussian)
      assertEquals(1, purelyRealInt.algebraicDegree)
      for (b <- -19 to 19 by 2) {
        val complexInt = new ImagQuadInt(a, b, ringEisenstein, 2)
        assertEquals(2, complexInt.algebraicDegree)
      }
    }
  }

  @Test def testTrace(): Unit = {
    println("trace")
    for (m <- -10 to 10) {
      val gaussianInt = new ImagQuadInt(m, 1, ringGaussian)
      val expected = 2L * m
      val actual = gaussianInt.trace
      val assertionMessage = "Trace of " + gaussianInt.toString + " ought to be " + expected.toString
      assertEquals(assertionMessage, expected, actual)
    }
    for (n <- -19 to 19 by 2) {
      val eisensteinInt = new ImagQuadInt(n, 1, ringEisenstein, 2)
      val actual = eisensteinInt.trace
      val assertionMessage = "Trace of " + eisensteinInt.toString + " ought to be " + n.toString
      assertEquals(assertionMessage, n, actual)
    }
  }

  @Test def testNorm(): Unit = {
    println("norm")
    var quadInt = new ImagQuadInt(12, 7, ringGaussian)
    var expected = 193L
    var actual = quadInt.norm
    var assertionMessage = "Norm of " + quadInt.toString + " ought to be " + expected.toString
    assertEquals(assertionMessage, expected, actual)
    quadInt = new ImagQuadInt(7, 12, ringGaussian)
    actual = quadInt.norm
    assertionMessage = "Norm of " + quadInt.toString + " ought to be " + expected.toString
    assertEquals(assertionMessage, expected, actual)
    quadInt = new ImagQuadInt(11, -6, ringZi2)
    actual = quadInt.norm
    assertionMessage = "Norm of " + quadInt.toString + " ought to be " + expected.toString
    assertEquals(assertionMessage, expected, actual)
    quadInt = new ImagQuadInt(25, 7, ringEisenstein, 2)
    actual = quadInt.norm
    assertionMessage = "Norm of " + quadInt.toString + " ought to be " + expected.toString
    assertEquals(assertionMessage, expected, actual)
    quadInt = new ImagQuadInt(-9, 4, ringOQi7)
    actual = quadInt.norm
    assertionMessage = "Norm of " + quadInt.toString + " ought to be " + expected.toString
    assertEquals(assertionMessage, expected, actual)
    quadInt = new ImagQuadInt(1, 1, ringRandom)
    expected = 1L - ringRandom.radicand
    actual = quadInt.norm
    assertionMessage = "Norm of " + quadInt.toString + " ought to be " + expected.toString
    assertEquals(assertionMessage, expected, actual)
  }

  @Test def testMinPolynomialCoeffs(): Unit = {
    println("minPolynomialCoeffs")
    val zeroIQI = new ImagQuadInt(0, 0, ringRandom)
    var expected = Array(0L, 1L, 0L)
    var actual = zeroIQI.minPolynomialCoeffs
    assertArrayEquals(expected, actual)
    for (a <- -19 to 19 by 2) {
      val purelyRealInt = new ImagQuadInt(a, 0, ringGaussian)
      expected = Array(-a, 1L, 0L)
      actual = purelyRealInt.minPolynomialCoeffs
      assertArrayEquals(expected, actual)
      for (b <- -19 to 19 by 2) {
        val complexInt = new ImagQuadInt(a, b, ringEisenstein, 2)
        val expNorm = (a * a + 3 * b * b)/4
        expected = Array(expNorm, -a, 1)
        actual = complexInt.minPolynomialCoeffs
        assertArrayEquals(expected, actual)
      }
    }
  }

  @Test def testMinPolynomialString(): Unit = {
    println("minPolynomialString")
    val zeroIQI = new ImagQuadInt(0, 0, ringRandom)
    var expected = "x"
    var actual = zeroIQI.minPolynomialString.replace(" ", "")
    assertEquals(expected, actual)
    for (a <- -19 to -1 by 2) {
      val purelyRealInt = new ImagQuadInt(a, 0, ringGaussian)
      expected = "x+" + (-a).toString
      actual = purelyRealInt.minPolynomialString.replace(" ", "")
      assertEquals(expected, actual)
      for (b <- 1 to 19 by 2) {
        val complexInt = new ImagQuadInt(a, b, ringEisenstein, 2)
        val expNorm = (a * a + 3 * b * b)/4
        expected = "x^2+" + (-a).toString + "+" + expNorm.toString
        actual = complexInt.minPolynomialString.replace(" ", "")
        assertEquals(expected, actual)
      }
    }
  }

  @Test def testConjugate(): Unit = {
    println("conjugate")
    var testImagQuadInt = new ImagQuadInt(12, 7, ringGaussian)
    var expected = new ImagQuadInt(12, -7, ringGaussian)
    var actual = testImagQuadInt.conjugate
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(7, 0, ringEisenstein)
    actual = testImagQuadInt.conjugate
    assertEquals(testImagQuadInt, actual)
    testImagQuadInt = new ImagQuadInt(0, 1, ringOQi7)
    expected = new ImagQuadInt(0, -1, ringOQi7)
    actual = testImagQuadInt.conjugate
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(-25, -3, ringOQi19, 2)
    expected = new ImagQuadInt(-25, 3, ringOQi19, 2)
    actual = testImagQuadInt.conjugate
    assertEquals(expected, actual)
  }

  @Test def testEquals(): Unit = {
    println("equals")
    var testImagQuadIntA = new ImagQuadInt(1, 1, ringGaussian)
    var testImagQuadIntB = new ImagQuadInt(1, 1, ringGaussian)
    assertEquals(testImagQuadIntA, testImagQuadIntB)
    testImagQuadIntB = new ImagQuadInt(1, 1, ringZi2)
    assertNotEquals(testImagQuadIntA, testImagQuadIntB)
    testImagQuadIntA = new ImagQuadInt(7, 0, ringEisenstein)
    testImagQuadIntB = new ImagQuadInt(7, 0, ringOQi7)
    assertEquals(testImagQuadIntA, testImagQuadIntB)
    testImagQuadIntA = new ImagQuadInt(25, 3, ringOQi19)
    testImagQuadIntB = new ImagQuadInt(25, 3, ringOQi19, 2)
    assertNotEquals(testImagQuadIntA, testImagQuadIntB)
    testImagQuadIntA = new ImagQuadInt(-25, -3, ringOQi19, -2)
    assertEquals(testImagQuadIntA, testImagQuadIntB)
    testImagQuadIntA = new ImagQuadInt(2, -3, ringZi21)
    testImagQuadIntB = new ImagQuadInt(-2, 3, ringZi21, -1)
    assertEquals(testImagQuadIntA, testImagQuadIntB)
    testImagQuadIntA = new ImagQuadInt(-7, -2, ringZi37)
    testImagQuadIntB = new ImagQuadInt(7, 2, ringZi37, -1)
    assertEquals(testImagQuadIntA, testImagQuadIntB)
  }

  @Test def testHashCode(): Unit = {
    println("hashCode")
    import scala.collection.mutable.ArrayBuffer
    var numberList = new ArrayBuffer[ImagQuadInt]()
    var hashCodeList = new ArrayBuffer[Int]()
    for (d <- -19 to -1) {
      if (NumberTheoreticFunctionsCalculator.isSquarefree(d)) {
        val ring = new ImagQuadRing(d)
        for {
          a <- -10 to 10
          b <- 1 to 20
        } {
          val num = new ImagQuadInt(a, b, ring)
          numberList += num
          hashCodeList += num.hashCode
        }
        if (ring.hasHalfIntegers) {
          for {
            a <- -9 to 9 by 2
            b <- -9 to 9 by 2
          } {
            val num = new ImagQuadInt(a, b, ring, 2)
            numberList += num
            hashCodeList += num.hashCode
          }
        }
      }
    }
    val hashCodeSet = hashCodeList.toSet
    assertEquals(numberList.size, hashCodeSet.size)
  }

  @Test def testToString(): Unit = {
    println("toString")
    var testImagQuadInt = new ImagQuadInt(12, 7, ringGaussian)
    var expected = "12+7i"
    var actual = testImagQuadInt.toString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(11, -6, ringZi2)
    expected = "11-6sqrt(-2)"
    actual = testImagQuadInt.toString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(0, 1, ringEisenstein)
    expected = "sqrt(-3)"
    actual = testImagQuadInt.toString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(25, 7, ringEisenstein, 2)
    expected = "25/2+7sqrt(-3)/2"
    actual = testImagQuadInt.toString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(-9, 4, ringOQi7)
    expected = "-9+4sqrt(-7)"
    actual = testImagQuadInt.toString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(25, 3, ringOQi19, 2)
    expected = "25/2+3sqrt(-19)/2"
    actual = testImagQuadInt.toString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(2, 3, ringZi21)
    expected = "2+3sqrt(-21)"
    actual = testImagQuadInt.toString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(-7, -2, ringZi37)
    expected = "-7-2sqrt(-37)"
    actual = testImagQuadInt.toString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(-1, 197, ringRandom)
    expected = "-1+197sqrt(" + testDiscr.toString + ")"
    actual = testImagQuadInt.toString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(0, -1, ringGaussian)
    expected = "-i"
    actual = testImagQuadInt.toString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(0, 0, ringZi2)
    expected = "0"
    actual = testImagQuadInt.toString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(-3, 0, ringEisenstein)
    expected = "-3"
    actual = testImagQuadInt.toString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToStringAlt(): Unit = {
    println("toStringAlt")
    var testImagQuadInt = new ImagQuadInt(12, 7, ringGaussian)
    val assertionMessage = "toString and toStringAlt should be the same for rings without \"half-integers\""
    assertEquals(assertionMessage, testImagQuadInt.toString, testImagQuadInt.toStringAlt)
    testImagQuadInt = new ImagQuadInt(-1, 1, ringEisenstein, 2)
    var expected = "omega"
    var actual = testImagQuadInt.toStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(1, -1, ringEisenstein, 2)
    expected = "-omega"
    actual = testImagQuadInt.toStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(1, -1, ringEisenstein)
    expected = "-2omega"
    actual = testImagQuadInt.toStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(1, 1, ringOQi7, 2)
    expected = "theta"
    actual = testImagQuadInt.toStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(3, 1, ringOQi19, 2)
    expected = "1+theta"
    actual = testImagQuadInt.toStringAlt.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToUnicodeString(): Unit = {
    println("toUnicodeString")
    var testImagQuadInt = new ImagQuadInt(12, 7, ringGaussian)
    var expected = "12+7i"
    var actual = testImagQuadInt.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(11, -6, ringZi2)
    expected = "11\u22126\u221A(\u22122)"
    actual = testImagQuadInt.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(0, 1, ringEisenstein)
    expected = "\u221A(\u22123)"
    actual = testImagQuadInt.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(25, 7, ringEisenstein, 2)
    expected = "25/2+7\u221A(\u22123)/2"
    actual = testImagQuadInt.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(-9, 4, ringOQi7)
    expected = "\u22129+4\u221A(\u22127)"
    actual = testImagQuadInt.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(25, 3, ringOQi19, 2)
    expected = "25/2+3\u221A(\u221219)/2"
    actual = testImagQuadInt.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(2, 3, ringZi21)
    expected = "2+3\u221A(\u221221)"
    actual = testImagQuadInt.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(-7, -2, ringZi37)
    expected = "\u22127\u22122\u221A(\u221237)"
    actual = testImagQuadInt.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(-1, 197, ringRandom)
    expected = "\u22121+197\u221A(\u2212" + (-testDiscr).toString + ")"
    actual = testImagQuadInt.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(0, -1, ringGaussian)
    expected = "\u2212i"
    actual = testImagQuadInt.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(0, 0, ringZi2)
    expected = "0"
    actual = testImagQuadInt.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(-3, 0, ringEisenstein)
    expected = "\u22123"
    actual = testImagQuadInt.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToUnicodeStringAlt(): Unit = {
    println("toUnicodeStringAlt")
    var testImagQuadInt = new ImagQuadInt(12, 7, ringGaussian)
    val assertionMessage = "toUnicodeString and toUnicodeStringAlt should be the same for rings without \"half-integers\""
    assertEquals(assertionMessage, testImagQuadInt.toUnicodeString, testImagQuadInt.toUnicodeStringAlt)
    testImagQuadInt = new ImagQuadInt(-1, 1, ringEisenstein, 2)
    var expected = "\u03C9"
    var actual = testImagQuadInt.toUnicodeStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(1, -1, ringEisenstein, 2)
    expected = "\u2212\u03C9"
    actual = testImagQuadInt.toUnicodeStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(1, -1, ringEisenstein)
    expected = "\u22122\u03C9"
    actual = testImagQuadInt.toUnicodeStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(1, 1, ringOQi7, 2)
    expected = "\u03B8"
    actual = testImagQuadInt.toUnicodeStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(3, 1, ringOQi19, 2)
    expected = "1+\u03B8"
    actual = testImagQuadInt.toUnicodeStringAlt.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToTeXString(): Unit = {
    println("toTeXString")
    var testImagQuadInt = new ImagQuadInt(12, 7, ringGaussian)
    var expected = "12+7i"
    var actual = testImagQuadInt.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(11, -6, ringZi2)
    expected = "11-6\\sqrt{-2}"
    actual = testImagQuadInt.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(0, 1, ringEisenstein)
    expected = "\\sqrt{-3}"
    actual = testImagQuadInt.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(25, 7, ringEisenstein, 2)
    expected = "\\frac{25}{2}+\\frac{7\\sqrt{-3}}{2}"
    actual = testImagQuadInt.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(-9, 4, ringOQi7)
    expected = "-9+4\\sqrt{-7}"
    actual = testImagQuadInt.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(25, 3, ringOQi19, 2)
    expected = "\\frac{25}{2}+\\frac{3\\sqrt{-19}}{2}"
    actual = testImagQuadInt.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(2, 3, ringZi21)
    expected = "2+3\\sqrt{-21}"
    actual = testImagQuadInt.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(-7, -2, ringZi37)
    expected = "-7-2\\sqrt{-37}"
    actual = testImagQuadInt.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(-1, 197, ringRandom)
    expected = "-1+197\\sqrt{" + testDiscr.toString + "}"
    actual = testImagQuadInt.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(0, -1, ringGaussian)
    expected = "-i"
    actual = testImagQuadInt.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(0, 0, ringZi2)
    expected = "0"
    actual = testImagQuadInt.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(-3, 0, ringEisenstein)
    expected = "-3"
    actual = testImagQuadInt.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToTeXStringAlt(): Unit = {
    println("toTeXStringAlt")
    var testImagQuadInt = new ImagQuadInt(12, 7, ringGaussian)
    val assertionMessage = "toTeXString and toTeXStringAlt should be the same for rings without \"half-integers\""
    assertEquals(assertionMessage, testImagQuadInt.toTeXString, testImagQuadInt.toTeXStringAlt)
    testImagQuadInt = new ImagQuadInt(-1, 1, ringEisenstein, 2)
    var expected = "\\omega"
    var actual = testImagQuadInt.toTeXStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(1, -1, ringEisenstein, 2)
    expected = "-\\omega"
    actual = testImagQuadInt.toTeXStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(1, -1, ringEisenstein)
    expected = "-2\\omega"
    actual = testImagQuadInt.toTeXStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(1, 1, ringOQi7, 2)
    expected = "\\theta"
    actual = testImagQuadInt.toTeXStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(3, 1, ringOQi19, 2)
    expected = "1+\\theta"
    actual = testImagQuadInt.toTeXStringAlt.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToHTMLString(): Unit = {
    println("toHTMLString")
    var testImagQuadInt = new ImagQuadInt(12, 7, ringGaussian)
    var expected = "12+7<i>i</i>"
    var actual = testImagQuadInt.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(11, -6, ringZi2)
    expected = "11&minus;6&radic;(&minus;2)"
    actual = testImagQuadInt.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(0, 1, ringEisenstein)
    expected = "&radic;(&minus;3)"
    actual = testImagQuadInt.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(25, 7, ringEisenstein, 2)
    expected = "25/2+7&radic;(&minus;3)/2"
    actual = testImagQuadInt.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(-9, 4, ringOQi7)
    expected = "&minus;9+4&radic;(&minus;7)"
    actual = testImagQuadInt.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(25, 3, ringOQi19, 2)
    expected = "25/2+3&radic;(&minus;19)/2"
    actual = testImagQuadInt.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(2, 3, ringZi21)
    expected = "2+3&radic;(&minus;21)"
    actual = testImagQuadInt.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(-7, -2, ringZi37)
    expected = "&minus;7&minus;2&radic;(&minus;37)"
    actual = testImagQuadInt.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(-1, 197, ringRandom)
    expected = "&minus;1+197&radic;(&minus;" + (-testDiscr).toString + ")"
    actual = testImagQuadInt.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(0, -1, ringGaussian)
    expected = "&minus;<i>i</i>"
    actual = testImagQuadInt.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(0, 0, ringZi2)
    expected = "0"
    actual = testImagQuadInt.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(-3, 0, ringEisenstein)
    expected = "&minus;3"
    actual = testImagQuadInt.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToHTMLStringAlt(): Unit = {
    println("toHTMLStringAlt")
    var testImagQuadInt = new ImagQuadInt(12, 7, ringGaussian)
    val assertionMessage = "toHTMLString and toHTMLStringAlt should be the same for rings without \"half-integers\""
    assertEquals(assertionMessage, testImagQuadInt.toHTMLString, testImagQuadInt.toHTMLStringAlt)
    testImagQuadInt = new ImagQuadInt(-1, 1, ringEisenstein, 2)
    var expected = "&omega;"
    var actual = testImagQuadInt.toHTMLStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(1, -1, ringEisenstein, 2)
    expected = "&minus;&omega;"
    actual = testImagQuadInt.toHTMLStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(1, -1, ringEisenstein)
    expected = "&minus;2&omega;"
    actual = testImagQuadInt.toHTMLStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(1, 1, ringOQi7, 2)
    expected = "&theta;"
    actual = testImagQuadInt.toHTMLStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testImagQuadInt = new ImagQuadInt(3, 1, ringOQi19, 2)
    expected = "1+&theta;"
    actual = testImagQuadInt.toHTMLStringAlt.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testAbs(): Unit = {
    println("abs")
    var testImagQuadInt = new ImagQuadInt(1, 1, ringGaussian)
    var expected = Math.sqrt(2)
    var actual = testImagQuadInt.abs
    assertEquals(expected, actual, QuadIntTest.TEST_DELTA)
    testImagQuadInt = new ImagQuadInt(1, -1, ringEisenstein)
    expected = 2.0
    actual = testImagQuadInt.abs
    assertEquals(expected, actual, QuadIntTest.TEST_DELTA)
    testImagQuadInt = new ImagQuadInt(-1, 1, ringEisenstein, 2)
    expected = 1.0
    actual = testImagQuadInt.abs
    assertEquals(expected, actual, QuadIntTest.TEST_DELTA)
    testImagQuadInt = new ImagQuadInt(-5, 2, ringRandom)
    expected = Math.abs(Math.sqrt(25.0 - 4 * ringRandom.radicand))
    actual = testImagQuadInt.abs
    assertEquals(expected, actual, QuadIntTest.TEST_DELTA)
  }

  @Test def testAngle(): Unit = {
    println("angle")
    val imagUnit = new ImagQuadInt(0, 1, ringGaussian)
    var expected = Math.PI/2
    var actual = imagUnit.angle
    assertEquals(expected, actual, QuadIntTest.TEST_DELTA)
    val omega = new ImagQuadInt(-1, 1, ringEisenstein, 2)
    expected = 2 * Math.PI/3
    actual = omega.angle
    assertEquals(expected, actual, QuadIntTest.TEST_DELTA)
    val negImagUnit = new ImagQuadInt(0, -1, ringGaussian)
    expected = -Math.PI/2
    actual = negImagUnit.angle
    assertEquals(expected, actual, QuadIntTest.TEST_DELTA)
  }

  @Test def testGetRealPartNumeric(): Unit = {
    println("getRealPartNumeric")
    var testQuadInt = new ImagQuadInt(7, 3, ringEisenstein, 2)
    var expected = 3.5
    var actual = testQuadInt.getRealPartNumeric
    assertEquals(expected, actual, QuadIntTest.TEST_DELTA)
    testQuadInt = new ImagQuadInt(5, 8, ringRandom)
    expected = 5.0
    actual = testQuadInt.getRealPartNumeric
    assertEquals(expected, actual, QuadIntTest.TEST_DELTA)
  }

  @Test def testGetImagPartNumeric(): Unit = {
    println("getImagPartNumeric")
    var testQuadInt = new ImagQuadInt(7, 3, ringEisenstein, 2)
    var expected = 3.0 * Math.sqrt(3) / 2.0
    var actual = testQuadInt.getImagPartNumeric
    assertEquals(expected, actual, QuadIntTest.TEST_DELTA)
    testQuadInt = new ImagQuadInt(5, 8, ringRandom)
    expected = 8.0 * ringRandom.getAbsNegRadSqrt
    actual = testQuadInt.getImagPartNumeric
    assertEquals(expected, actual, QuadIntTest.TEST_DELTA)
  }

  @Test def testPlus(): Unit = {
    println("+")
    var testAddendA = new ImagQuadInt(12, 7, ringGaussian)
    var testAddendB = new ImagQuadInt(-5, 5, ringGaussian)
    var expected = new ImagQuadInt(7, 12, ringGaussian)
    var actual = testAddendA + testAddendB
    assertEquals(expected, actual)
    testAddendA = new ImagQuadInt(11, -6, ringZi2)
    testAddendB = new ImagQuadInt(3, 8, ringZi2)
    expected = new ImagQuadInt(14, 2, ringZi2)
    actual = testAddendA + testAddendB
    assertEquals(expected, actual)
    testAddendA = new ImagQuadInt(25, 7, ringEisenstein, 2)
    testAddendB = new ImagQuadInt(1, 1, ringEisenstein, 2)
    expected = new ImagQuadInt(13, 4, ringEisenstein)
    actual = testAddendA + testAddendB
    assertEquals(expected, actual)
    testAddendA = new ImagQuadInt(-9, 4, ringOQi7)
    testAddendB = new ImagQuadInt(-1, 1, ringOQi7, 2)
    expected = new ImagQuadInt(-19, 9, ringOQi7, 2)
    actual = testAddendA + testAddendB
    assertEquals(expected, actual)
    testAddendA = new ImagQuadInt(4, 7, ringRandom)
    testAddendB = new ImagQuadInt(8, 3, ringRandom)
    expected = new ImagQuadInt(12, 10, ringRandom)
    actual = testAddendA + testAddendB
    assertEquals(expected, actual)
  }

  @Test def testPlusAdditiveInverses(): Unit = {
    val expected = new ImagQuadInt(0, 0, ringRandom)
    for {a <- -10 to 10
         b <- -10 to 10} {
      val testAddendA = new ImagQuadInt(a, b, ringRandom)
      val testAddendB = new ImagQuadInt(-a, -b, ringRandom)
      val actual = testAddendA + testAddendB
      assertEquals(expected, actual)
    }
  }

  @Test def testPlusToQuarticDomain(): Unit = {
    val testAddendA = new ImagQuadInt(5, 3, ringGaussian)
    val testAddendB = new ImagQuadInt(7, 2, ringRandom)
    try {
      val result = testAddendA + testAddendB
      val failMsg = "Trying to add " + testAddendA.toString + " to " + testAddendB.toString + " should have caused an exception, not given result " + result.toString
      fail(failMsg)
    } catch {
      case adoe: AlgebraicDegreeOverflowException => println("Trying to add " + testAddendA.toString + " to " + testAddendB.toString + " correctly caused AlgebraicDegreeOverflowException")
        println("\"" + adoe.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is the wrong exception for trying to add " + testAddendA.toString + " to " + testAddendB.toString
        fail(failMsg)
    }
  }

  @Test def testNegate(): Unit = {
    println("unary_-")
    for {a <- -10 to 10
         b <- -10 to 10} {
      val testNumber = new ImagQuadInt(a, b, ringRandom)
      val expected = new ImagQuadInt(-a, -b, ringRandom)
      val actual = -testNumber
      assertEquals(expected, actual)
    }
  }

  @Test def testMinus(): Unit = {
    println("-")
    var minuend = new ImagQuadInt(12, 7, ringGaussian)
    var subtrahend = new ImagQuadInt(-5, 5, ringGaussian)
    var expected = new ImagQuadInt(17, 2, ringGaussian)
    var actual = minuend - subtrahend
    assertEquals(expected, actual)
    minuend = new ImagQuadInt(11, -6, ringZi2)
    subtrahend = new ImagQuadInt(3, 8, ringZi2)
    expected = new ImagQuadInt(8, -14, ringZi2)
    actual = minuend - subtrahend
    assertEquals(expected, actual)
    minuend = new ImagQuadInt(25, 7, ringEisenstein, 2)
    subtrahend = new ImagQuadInt(1, 1, ringEisenstein, 2)
    expected = new ImagQuadInt(12, 3, ringEisenstein)
    actual = minuend - subtrahend
    assertEquals(expected, actual)
    minuend = new ImagQuadInt(-9, 4, ringOQi7)
    subtrahend = new ImagQuadInt(-1, 1, ringOQi7, 2)
    expected = new ImagQuadInt(-17, 7, ringOQi7, 2)
    actual = minuend - subtrahend
    assertEquals(expected, actual)
    minuend = new ImagQuadInt(4, 7, ringRandom)
    subtrahend = new ImagQuadInt(8, 3, ringRandom)
    expected = new ImagQuadInt(-4, 4, ringRandom)
    actual = minuend - subtrahend
    assertEquals(expected, actual)
  }

  @Test def testTimes(): Unit = {
    println("*")
    var testMultiplicandA = new ImagQuadInt(12, 7, ringGaussian)
    var testMultiplicandB = new ImagQuadInt(-5, 5, ringGaussian)
    var expected = new ImagQuadInt(-95, 25, ringGaussian)
    var actual = testMultiplicandA * testMultiplicandB
    assertEquals(expected, actual)
    testMultiplicandA = new ImagQuadInt(11, -6, ringZi2)
    testMultiplicandB = new ImagQuadInt(3, 8, ringZi2)
    expected = new ImagQuadInt(129, 70, ringZi2)
    actual = testMultiplicandA * testMultiplicandB
    assertEquals(expected, actual)
    testMultiplicandA = new ImagQuadInt(25, 7, ringEisenstein, 2)
    testMultiplicandB = new ImagQuadInt(1, 1, ringEisenstein, 2)
    expected = new ImagQuadInt(1, 8, ringEisenstein)
    actual = testMultiplicandA * testMultiplicandB
    assertEquals(expected, actual)
    testMultiplicandA = new ImagQuadInt(-9, 4, ringOQi7)
    testMultiplicandB = new ImagQuadInt(-1, 1, ringOQi7, 2)
    expected = new ImagQuadInt(-19, -13, ringOQi7, 2)
    actual = testMultiplicandA * testMultiplicandB
    assertEquals(expected, actual)
    testMultiplicandA = new ImagQuadInt(4, 7, ringRandom)
    testMultiplicandB = new ImagQuadInt(8, 3, ringRandom)
    val expRe = 32 + 21 * ringRandom.radicand
    expected = new ImagQuadInt(expRe, 68, ringRandom)
    actual = testMultiplicandA * testMultiplicandB
    assertEquals(expected, actual)
  }

  @Test def testDivides(): Unit = {
    println("/")
    var testDividend = new ImagQuadInt(-95, 25, ringGaussian)
    var testDivisor = new ImagQuadInt(-5, 5, ringGaussian)
    var expected = new ImagQuadInt(12, 7, ringGaussian)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case e: Exception => val failMsg = "Trying to divide " + testDividend.toString + " by " + testDivisor.toString + " should not have caused " + e.getClass.getName
        fail(failMsg)
    }
    testDivisor = expected
    expected = new ImagQuadInt(-5, 5, ringGaussian)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case e: Exception => val failMsg = "Trying to divide " + testDividend.toString + " by " + testDivisor.toString + " should not have caused " + e.getClass.getName
        fail(failMsg)
    }
    testDividend = new ImagQuadInt(129, 70, ringZi2)
    testDivisor = new ImagQuadInt(3, 8, ringZi2)
    expected = new ImagQuadInt(11, -6, ringZi2)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case e: Exception => val failMsg = "Trying to divide " + testDividend.toString + " by " + testDivisor.toString + " should not have caused " + e.getClass.getName
        fail(failMsg)
    }
    testDivisor = expected
    expected = new ImagQuadInt(3, 8, ringZi2)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case e: Exception => val failMsg = "Trying to divide " + testDividend.toString + " by " + testDivisor.toString + " should not have caused " + e.getClass.getName
        fail(failMsg)
    }
    testDividend = new ImagQuadInt(1, 8, ringEisenstein)
    testDivisor = new ImagQuadInt(1, 1, ringEisenstein, 2)
    expected = new ImagQuadInt(25, 7, ringEisenstein, 2)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case e: Exception => val failMsg = "Trying to divide " + testDividend.toString + " by " + testDivisor.toString + " should not have caused " + e.getClass.getName
        fail(failMsg)
    }
    testDivisor = expected
    expected = new ImagQuadInt(1, 1, ringEisenstein, 2)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case e: Exception => val failMsg = "Trying to divide " + testDividend.toString + " by " + testDivisor.toString + " should not have caused " + e.getClass.getName
        fail(failMsg)
    }
    testDividend = new ImagQuadInt(-19, -13, ringOQi7, 2)
    testDivisor = new ImagQuadInt(-1, 1, ringOQi7, 2)
    expected = new ImagQuadInt(-9, 4, ringOQi7)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case e: Exception => val failMsg = "Trying to divide " + testDividend.toString + " by " + testDivisor.toString + " should not have caused " + e.getClass.getName
        fail(failMsg)
    }
    testDivisor = expected
    expected = new ImagQuadInt(-1, 1, ringOQi7, 2)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case e: Exception => val failMsg = "Trying to divide " + testDividend.toString + " by " + testDivisor.toString + " should not have caused " + e.getClass.getName
        fail(failMsg)
    }
    val expRe = 32 + 21 * ringRandom.radicand
    testDividend = new ImagQuadInt(expRe, 68, ringRandom)
    testDivisor = new ImagQuadInt(8, 3, ringRandom)
    expected = new ImagQuadInt(4, 7, ringRandom)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case e: Exception => val failMsg = "Trying to divide " + testDividend.toString + " by " + testDivisor.toString + " should not have caused " + e.getClass.getName
        fail(failMsg)
    }
    testDivisor = expected
    expected = new ImagQuadInt(8, 3, ringRandom)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case e: Exception => val failMsg = "Trying to divide " + testDividend.toString + " by " + testDivisor.toString + " should not have caused " + e.getClass.getName
        fail(failMsg)
    }
  }

  @Test def testDivideByNonDivisor(): Unit = {
    for (d <- -19 to -1) {
      if (NumberTheoreticFunctionsCalculator.isSquarefree(d)) {
        val r = new ImagQuadRing(d)
        val testDividend = new ImagQuadInt(20, 20, r)
        val testNonDivisor = new ImagQuadInt(20, 19, r)
        try {
          val result = testDividend / testNonDivisor
          val failMsg = "Trying to divide " + testDividend.toString + " by " + testNonDivisor.toString + " should have triggered NotDivisibleException, not given result " + result.toString
          fail(failMsg)
        } catch {
          case nde: NotDivisibleException => println("Trying to divide " + testDividend.toString + " by " + testNonDivisor.toString + " correctly triggered NotDivisibleException")
            println("\"" + nde.getMessage + "\"")
          case e: Exception => println("\"" + e.getMessage + "\"")
            val failMsg = e.getClass.getName + " is wrong exception to throw for trying to divide " + testDividend.toString + " by " + testNonDivisor.toString
            fail(failMsg)
        }
      }
    }
  }

  @Test def testDivisionByZero(): Unit = {
    val testDividend = new ImagQuadInt(4, 7, ringRandom)
    val zero = new ImagQuadInt(0, 0, ringRandom)
    try {
      val result = testDividend / zero
      val failMsg = "Trying to divide " + testDividend.toString + " by 0 should have caused an exception, not given result " + result.toString
      fail(failMsg)
    } catch {
      case _: NotDivisibleException => val failMsg = "NotDivisibleException is the wrong exception to throw for division by zero"
        fail(failMsg)
      case iae: IllegalArgumentException => println("IllegalArgumentException is the preferred exception for division by zero")
        println("\"" + iae.getMessage + "\"")
      case ae: ArithmeticException => println("ArithmeticException is adequate for division by zero")
        println("\"" + ae.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is the wrong exception to throw for division by zero"
        fail(failMsg)
    }
  }

  @Test def testTo(): Unit = {
    println("to")
    var startPoint = new ImagQuadInt(-2, 1, ringGaussian)
    var endPoint = new ImagQuadInt(2, 5, ringGaussian)
    var step = new ImagQuadInt(1, 1, ringGaussian)
    var expected = new ImagQuadIntLine(startPoint, endPoint, step)
    var actual = startPoint.to(endPoint)
    assertEquals(expected, actual)
    startPoint = new ImagQuadInt(-2, 5, ringZi2)
    endPoint = new ImagQuadInt(-11, 8, ringZi2)
    step = new ImagQuadInt(-3, 1, ringZi2)
    expected = new ImagQuadIntLine(startPoint, endPoint, step)
    actual = startPoint.to(endPoint)
    assertEquals(expected, actual)
//    startPoint = new ImagQuadInt(-2, 5, ringZi2)
//    endPoint = new ImagQuadInt(-11, 8, ringZi2)
//    step = new ImagQuadInt(-3, 1, ringZi2)
//    expected = new ImagQuadIntLine(startPoint, endPoint, step)
//    actual = startPoint.to(endPoint)
//    assertEquals(expected, actual)
//    startPoint = new ImagQuadInt(-2, 5, ringZi2)
//    endPoint = new ImagQuadInt(-11, 8, ringZi2)
//    step = new ImagQuadInt(-3, 1, ringZi2)
//    expected = new ImagQuadIntLine(startPoint, endPoint, step)
//    actual = startPoint.to(endPoint)
//    assertEquals(expected, actual)
  }

  @Test def testToAlgDegOverflow(): Unit = {
    val startPoint = new ImagQuadInt(-2, 1, ringGaussian)
    val endPoint = new ImagQuadInt(-11, 8, ringZi2)
    try {
      val result = startPoint.to(endPoint)
      val failMsg = "Trying to make line from " + startPoint.toString + " to " + endPoint.toString + " should have caused an exception, not given result " + result.toString
      fail(failMsg)
    } catch {
      case adoe: AlgebraicDegreeOverflowException => println("Trying to make line from " + startPoint.toString + " to " + endPoint.toString + " correctly triggered AlgebraicDegreeOverflowException")
        println("\"" + adoe.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is not the right exception to throw for trying to make line from " + startPoint.toString + " to " + endPoint.toString
        fail(failMsg)
    }
  }
}
