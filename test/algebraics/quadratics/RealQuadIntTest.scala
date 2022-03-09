package algebraics.quadratics

import algebraics.{AlgebraicDegreeOverflowException, NotDivisibleException}
import calculators.NumberTheoreticFunctionsCalculator

import org.junit.Test
import org.junit.Assert._

class RealQuadIntTest {
  private val ringZ2 = new RealQuadRing(2)
  private val ringZPhi = new RealQuadRing(5)
  private val ringOQ13 = new RealQuadRing(13)
  private val testDiscr: Int = NumberTheoreticFunctionsCalculator.randomSquarefreeNumber(128) match {
    case 1 => 3
    case 5 => 6
    case n: Int => n
  }
  private val ringRandom = new RealQuadRing(testDiscr)

  @Test def testConstructor(): Unit = {
    try {
      val badQuadInt = new RealQuadInt(3, 1, ringZ2, 2)
      val failMsg = "Should not have been able to create " + badQuadInt.toString
      fail(failMsg)
    } catch {
      case iae: IllegalArgumentException => println("Trying to create 3/2 + sqrt(2)/2 as quadratic integer correctly triggered IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is not the right exception for trying to create 3/2 + sqrt(2)/2 as quadratic integer"
        fail(failMsg)
    }
    try {
      val badQuadInt = new RealQuadInt(5, 2, ringZPhi, 2)
      val failMsg = "Should not have been able to create " + badQuadInt.toString
      fail(failMsg)
    } catch {
      case iae: IllegalArgumentException => println("Trying to create 5/2 + sqrt(5) as quadratic integer correctly triggered IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is not the right exception for trying to create 5/2 + sqrt(5) as quadratic integer"
        fail(failMsg)
    }
    try {
      val badQuadInt = new RealQuadInt(5, 3, ringOQ13, 4)
      val failMsg = "Should not have been able to create " + badQuadInt.toString
      fail(failMsg)
    } catch {
      case iae: IllegalArgumentException => println("Trying to create 5/4 + 3sqrt(13)/4 as quadratic integer correctly triggered IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is not the right exception for trying to create 5/4 + 3sqrt(13)/4 as quadratic integer"
        fail(failMsg)
    }
  }

  @Test def testAlgebraicDegree(): Unit = {
    println("algebraicDegree")
    val zeroIQI = new RealQuadInt(0, 0, ringRandom)
    assertEquals(0, zeroIQI.algebraicDegree)
    for (a <- -19 to 19 by 2) {
      val rationalRealInt = new RealQuadInt(a, 0, ringZ2)
      assertEquals(1, rationalRealInt.algebraicDegree)
      for (b <- -19 to 19 by 2) {
        val realQuadInt = new RealQuadInt(a, b, ringZPhi, 2)
        assertEquals(2, realQuadInt.algebraicDegree)
      }
    }
  }

  @Test def testTrace(): Unit = {
    println("trace")
    for (m <- -10 to 10) {
      val quadInt = new RealQuadInt(m, 1, ringZ2)
      val expected = 2L * m
      val actual = quadInt.trace
      val assertionMessage = "Trace of " + quadInt.toString + " ought to be " + expected.toString
      assertEquals(assertionMessage, expected, actual)
    }
    for (n <- -19 to 19 by 2) {
      val quadInt = new RealQuadInt(n, 1, ringZPhi, 2)
      val actual = quadInt.trace
      val assertionMessage = "Trace of " + quadInt.toString + " ought to be " + n.toString
      assertEquals(assertionMessage, n, actual)
    }
  }

  @Test def testNorm(): Unit = {
    println("norm")
    var quadInt = new RealQuadInt(15, 4, ringZ2)
    var expected = 193L
    var actual = quadInt.norm
    var assertionMessage = "Norm of " + quadInt.toString + " ought to be " + expected.toString
    assertEquals(assertionMessage, expected, actual)
    quadInt = new RealQuadInt(0, -193, ringZPhi)
    expected = -186245L
    actual = quadInt.norm
    assertionMessage = "Norm of " + quadInt.toString + " ought to be " + expected.toString
    assertEquals(assertionMessage, expected, actual)
    quadInt = new RealQuadInt(-43, 9, ringOQ13, 2)
    expected = 199L
    actual = quadInt.norm
    assertionMessage = "Norm of " + quadInt.toString + " ought to be " + expected.toString
    assertEquals(assertionMessage, expected, actual)
    quadInt = new RealQuadInt(1, 1, ringRandom)
    expected = 1L - ringRandom.radicand
    actual = quadInt.norm
    assertionMessage = "Norm of " + quadInt.toString + " ought to be " + expected.toString
    assertEquals(assertionMessage, expected, actual)
  }

  @Test def testMinPolynomialCoeffs(): Unit = {
    println("minPolynomialCoeffs")
    val zeroIQI = new RealQuadInt(0, 0, ringRandom)
    var expected = Array(0L, 1L, 0L)
    var actual = zeroIQI.minPolynomialCoeffs
    assertArrayEquals(expected, actual)
    for (a <- -19 to 19 by 2) {
      val rationalRealInt = new RealQuadInt(a, 0, ringZ2)
      expected = Array(-a, 1L, 0L)
      actual = rationalRealInt.minPolynomialCoeffs
      assertArrayEquals(expected, actual)
      for (b <- -19 to 19 by 2) {
        val realQuadInt = new RealQuadInt(a, b, ringZPhi, 2)
        val expNorm = (a * a - 5 * b * b)/4
        expected = Array(expNorm, -a, 1)
        actual = realQuadInt.minPolynomialCoeffs
        assertArrayEquals(expected, actual)
      }
    }
  }

  @Test def testMinPolynomialString(): Unit = {
    println("minPolynomialString")
    val zeroIQI = new RealQuadInt(0, 0, ringRandom)
    var expected = "x"
    var actual = zeroIQI.minPolynomialString.replace(" ", "")
    assertEquals(expected, actual)
    for (a <- -19 to -1 by 2) {
      val rationalRealInt = new RealQuadInt(a, 0, ringZ2)
      expected = "x+" + (-a).toString
      actual = rationalRealInt.minPolynomialString.replace(" ", "")
      assertEquals(expected, actual)
      for (b <- 1 to 19 by 2) {
        val realQuadInt = new RealQuadInt(a, b, ringZPhi, 2)
        val expNorm = (a * a - 5 * b * b)/4
        expected = ("x^2+" + (-a).toString + "+" + expNorm.toString).replace("+-", "-")
        actual = realQuadInt.minPolynomialString.replace(" ", "")
        assertEquals(expected, actual)
      }
    }
  }

  @Test def testConjugate(): Unit = {
    println("conjugate")
    var testRealQuadInt = new RealQuadInt(0, -1, ringZ2)
    var expected = new RealQuadInt(0, 1, ringZ2)
    var actual = testRealQuadInt.conjugate
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(43, -9, ringOQ13, 2)
    expected = new RealQuadInt(43, 9, ringOQ13, 2)
    actual = testRealQuadInt.conjugate
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(-7, -8, ringRandom)
    expected = new RealQuadInt(-7, 8, ringRandom)
    actual = testRealQuadInt.conjugate
    assertEquals(expected, actual)
  }

  @Test def testEquals(): Unit = {
    println("equals")
    var testRealQuadIntA = new RealQuadInt(1, 1, ringZ2)
    var testRealQuadIntB = new RealQuadInt(1, 1, ringZ2)
    assertEquals(testRealQuadIntA, testRealQuadIntB)
    testRealQuadIntB = new RealQuadInt(1, 1, ringZPhi)
    assertNotEquals(testRealQuadIntA, testRealQuadIntB)
    testRealQuadIntA = new RealQuadInt(7, 0, ringOQ13)
    testRealQuadIntB = new RealQuadInt(7, 0, ringRandom)
    assertEquals(testRealQuadIntA, testRealQuadIntB)
    testRealQuadIntA = new RealQuadInt(-43, 9, ringOQ13, -2)
    testRealQuadIntB = new RealQuadInt(43, -9, ringOQ13, 2)
    assertEquals(testRealQuadIntA, testRealQuadIntB)
    testRealQuadIntA = new RealQuadInt(7, 8, ringRandom, -1)
    testRealQuadIntB = new RealQuadInt(-7, -8, ringRandom)
  }

  @Test def testHashCode(): Unit = {
    println("hashCode")
    import scala.collection.mutable.ArrayBuffer
    var numberList = new ArrayBuffer[RealQuadInt]()
    var hashCodeList = new ArrayBuffer[Int]()
    for (d <- 2 to 19) {
      if (NumberTheoreticFunctionsCalculator.isSquarefree(d)) {
        val ring = new RealQuadRing(d)
        for {
          a <- -10 to 10
          b <- 1 to 20
        } {
          val num = new RealQuadInt(a, b, ring)
          numberList += num
          hashCodeList += num.hashCode
        }
        if (ring.hasHalfIntegers) {
          for {
            a <- -9 to 9 by 2
            b <- -9 to 9 by 2
          } {
            val num = new RealQuadInt(a, b, ring, 2)
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
    var testRealQuadInt = new RealQuadInt(15, 4, ringZ2)
    var expected = "15+4sqrt(2)"
    var actual = testRealQuadInt.toString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(0, -193, ringZPhi)
    expected = "-193sqrt(5)"
    actual = testRealQuadInt.toString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(-43, 9, ringOQ13, 2)
    expected = "-43/2+9sqrt(13)/2"
    actual = testRealQuadInt.toString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(-1, -197, ringRandom)
    expected = "-1-197sqrt(" + testDiscr.toString + ")"
    actual = testRealQuadInt.toString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(0, -1, ringZ2)
    expected = "-sqrt(2)"
    actual = testRealQuadInt.toString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(1, 1, ringZPhi, 2)
    expected = "1/2+sqrt(5)/2"
    actual = testRealQuadInt.toString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(0, 1, ringOQ13)
    expected = "sqrt(13)"
    actual = testRealQuadInt.toString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(3, 1, ringRandom)
    expected = "3+sqrt(" + testDiscr + ")"
    actual = testRealQuadInt.toString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToStringAlt(): Unit = {
    println("toStringAlt")
    var testRealQuadInt = new RealQuadInt(15, 4, ringZ2)
    val assertionMessage = "toString and toStringAlt should be the same for rings without \"half-integers\""
    assertEquals(assertionMessage, testRealQuadInt.toString, testRealQuadInt.toStringAlt)
    testRealQuadInt = new RealQuadInt(1, 1, ringZPhi, 2)
    var expected = "phi"
    var actual = testRealQuadInt.toStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(-1, -1, ringZPhi, 2)
    expected = "-phi"
    actual = testRealQuadInt.toStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(-1, -1, ringZPhi)
    expected = "-2phi"
    actual = testRealQuadInt.toStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(1, 1, ringOQ13, 2)
    expected = "theta"
    actual = testRealQuadInt.toStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(1, -1, ringOQ13, 2)
    expected = "1-theta"
    actual = testRealQuadInt.toStringAlt.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToUnicodeString(): Unit = {
    println("toUnicodeString")
    var testRealQuadInt = new RealQuadInt(15, 4, ringZ2)
    var expected = "15+4\u221A(2)"
    var actual = testRealQuadInt.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(0, -193, ringZPhi)
    expected = "\u2212193\u221A(5)"
    actual = testRealQuadInt.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(-43, 9, ringOQ13, 2)
    expected = "\u221243/2+9\u221A(13)/2"
    actual = testRealQuadInt.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(-1, -197, ringRandom)
    expected = "\u22121\u2212197\u221A(" + testDiscr.toString + ")"
    actual = testRealQuadInt.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(0, -1, ringZ2)
    expected = "\u2212\u221A(2)"
    actual = testRealQuadInt.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(1, 1, ringZPhi, 2)
    expected = "1/2+\u221A(5)/2"
    actual = testRealQuadInt.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(0, 1, ringOQ13)
    expected = "\u221A(13)"
    actual = testRealQuadInt.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(3, 1, ringRandom)
    expected = "3+\u221A(" + testDiscr + ")"
    actual = testRealQuadInt.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToUnicodeStringAlt(): Unit = {
    println("toUnicodeStringAlt")
    var testRealQuadInt = new RealQuadInt(15, 4, ringZ2)
    val assertionMessage = "toUnicodeString and toUnicodeStringAlt should be the same for rings without \"half-integers\""
    assertEquals(assertionMessage, testRealQuadInt.toUnicodeString, testRealQuadInt.toUnicodeStringAlt)
    testRealQuadInt = new RealQuadInt(1, 1, ringZPhi, 2)
    var expected = "\u03C6"
    var actual = testRealQuadInt.toUnicodeStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(-1, -1, ringZPhi, 2)
    expected = "\u2212\u03C6"
    actual = testRealQuadInt.toUnicodeStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(-1, -1, ringZPhi)
    expected = "\u22122\u03C6"
    actual = testRealQuadInt.toUnicodeStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(1, 1, ringOQ13, 2)
    expected = "\u03B8"
    actual = testRealQuadInt.toUnicodeStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(1, -1, ringOQ13, 2)
    expected = "1\u2212\u03B8"
    actual = testRealQuadInt.toUnicodeStringAlt.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToTeXString(): Unit = {
    println("toTeXString")
    var testRealQuadInt = new RealQuadInt(15, 4, ringZ2)
    var expected = "15+4\\sqrt{2}"
    var actual = testRealQuadInt.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(0, -193, ringZPhi)
    expected = "-193\\sqrt{5}"
    actual = testRealQuadInt.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(-43, 9, ringOQ13, 2)
    expected = "-\\frac{43}{2}+\\frac{9\\sqrt{13}}{2}"
    actual = testRealQuadInt.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(-1, -197, ringRandom)
    expected = "-1-197\\sqrt{" + testDiscr.toString + "}"
    actual = testRealQuadInt.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(0, -1, ringZ2)
    expected = "-\\sqrt{2}"
    actual = testRealQuadInt.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(1, 1, ringZPhi, 2)
    expected = "\\frac{1}{2}+\\frac{\\sqrt{5}}{2}"
    actual = testRealQuadInt.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(0, 1, ringOQ13)
    expected = "\\sqrt{13}"
    actual = testRealQuadInt.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(3, 1, ringRandom)
    expected = "3+\\sqrt{" + testDiscr + "}"
    actual = testRealQuadInt.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToTeXStringAlt(): Unit = {
    println("toTeXStringAlt")
    var testRealQuadInt = new RealQuadInt(15, 4, ringZ2)
    val assertionMessage = "toTeXString and toTeXStringAlt should be the same for rings without \"half-integers\""
    assertEquals(assertionMessage, testRealQuadInt.toTeXString, testRealQuadInt.toTeXStringAlt)
    testRealQuadInt = new RealQuadInt(1, 1, ringZPhi, 2)
    var expected = "\\phi"
    var actual = testRealQuadInt.toTeXStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(-1, -1, ringZPhi, 2)
    expected = "-\\phi"
    actual = testRealQuadInt.toTeXStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(-1, -1, ringZPhi)
    expected = "-2\\phi"
    actual = testRealQuadInt.toTeXStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(1, 1, ringOQ13, 2)
    expected = "\\theta"
    actual = testRealQuadInt.toTeXStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(1, -1, ringOQ13, 2)
    expected = "1-\\theta"
    actual = testRealQuadInt.toTeXStringAlt.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToHTMLString(): Unit = {
    println("toHTMLString")
    var testRealQuadInt = new RealQuadInt(15, 4, ringZ2)
    var expected = "15+4&radic;(2)"
    var actual = testRealQuadInt.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(0, -193, ringZPhi)
    expected = "&minus;193&radic;(5)"
    actual = testRealQuadInt.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(-43, 9, ringOQ13, 2)
    expected = "&minus;43/2+9&radic;(13)/2"
    actual = testRealQuadInt.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(-1, -197, ringRandom)
    expected = "&minus;1&minus;197&radic;(" + testDiscr.toString + ")"
    actual = testRealQuadInt.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(0, -1, ringZ2)
    expected = "&minus;&radic;(2)"
    actual = testRealQuadInt.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(1, 1, ringZPhi, 2)
    expected = "1/2+&radic;(5)/2"
    actual = testRealQuadInt.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(0, 1, ringOQ13)
    expected = "&radic;(13)"
    actual = testRealQuadInt.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(3, 1, ringRandom)
    expected = "3+&radic;(" + testDiscr + ")"
    actual = testRealQuadInt.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToHTMLStringAlt(): Unit = {
    println("toHTMLStringAlt")
    var testRealQuadInt = new RealQuadInt(15, 4, ringZ2)
    val assertionMessage = "toHTMLString and toHTMLStringAlt should be the same for rings without \"half-integers\""
    assertEquals(assertionMessage, testRealQuadInt.toHTMLString, testRealQuadInt.toHTMLStringAlt)
    testRealQuadInt = new RealQuadInt(1, 1, ringZPhi, 2)
    var expected = "&phi;"
    var actual = testRealQuadInt.toHTMLStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(-1, -1, ringZPhi, 2)
    expected = "&minus;&phi;"
    actual = testRealQuadInt.toHTMLStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(-1, -1, ringZPhi)
    expected = "&minus;2&phi;"
    actual = testRealQuadInt.toHTMLStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(1, 1, ringOQ13, 2)
    expected = "&theta;"
    actual = testRealQuadInt.toHTMLStringAlt.replace(" ", "")
    assertEquals(expected, actual)
    testRealQuadInt = new RealQuadInt(1, -1, ringOQ13, 2)
    expected = "1&minus;&theta;"
    actual = testRealQuadInt.toHTMLStringAlt.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testAbs(): Unit = {
    println("abs")
    var testRealQuadInt = new RealQuadInt(1, 1, ringZ2)
    var expected = 1.0 + Math.sqrt(2)
    var actual = testRealQuadInt.abs
    assertEquals(expected, actual, QuadIntTest.TEST_DELTA)
    testRealQuadInt = new RealQuadInt(1, -1, ringZ2)
    expected = Math.abs(1.0 - Math.sqrt(2))
    actual = testRealQuadInt.abs
    assertEquals(expected, actual, QuadIntTest.TEST_DELTA)
    testRealQuadInt = new RealQuadInt(-3, 1, ringOQ13, 2)
    expected = -1.5 + Math.sqrt(13)/2
    actual = testRealQuadInt.abs
    assertEquals(expected, actual, QuadIntTest.TEST_DELTA)
    testRealQuadInt = new RealQuadInt(-5, 2, ringRandom)
    expected = Math.abs(-5.0 + 2 * Math.sqrt(ringRandom.radicand))
    actual = testRealQuadInt.abs
    assertEquals(expected, actual, QuadIntTest.TEST_DELTA)
  }

  @Test def testAngle(): Unit = {
    println("angle")
    val goldenRatio = new RealQuadInt(1, 1, ringZPhi, 2)
    var actual = goldenRatio.angle
    assertEquals(0.0, actual, QuadIntTest.TEST_DELTA)
    val negNum = new RealQuadInt(-8, -7, ringRandom)
    actual = negNum.angle
    assertEquals(Math.PI, actual, QuadIntTest.TEST_DELTA)
  }

  @Test def testGetRealPartNumeric(): Unit = {
    println("getRealPartNumeric")
    var testRealQuadInt = new RealQuadInt(1, 1, ringZ2)
    var expected = 1.0 + Math.sqrt(2)
    var actual = testRealQuadInt.getRealPartNumeric
    assertEquals(expected, actual, QuadIntTest.TEST_DELTA)
    testRealQuadInt = new RealQuadInt(1, -1, ringZ2)
    expected = 1.0 - Math.sqrt(2)
    actual = testRealQuadInt.getRealPartNumeric
    assertEquals(expected, actual, QuadIntTest.TEST_DELTA)
    testRealQuadInt = new RealQuadInt(-3, 1, ringOQ13, 2)
    expected = -1.5 + Math.sqrt(13)/2
    actual = testRealQuadInt.getRealPartNumeric
    assertEquals(expected, actual, QuadIntTest.TEST_DELTA)
    testRealQuadInt = new RealQuadInt(-5, 2, ringRandom)
    expected = -5.0 + 2 * Math.sqrt(ringRandom.radicand)
    actual = testRealQuadInt.getRealPartNumeric
    assertEquals(expected, actual, QuadIntTest.TEST_DELTA)
  }

  @Test def testGetImagPartNumeric(): Unit = {
    println("getImagPartNumeric")
    var testRealQuadInt = new RealQuadInt(1, 1, ringZ2)
    val expected = 0.0
    var actual = testRealQuadInt.getImagPartNumeric
    assertEquals(expected, actual, QuadIntTest.TEST_DELTA)
    testRealQuadInt = new RealQuadInt(1, -1, ringZ2)
    actual = testRealQuadInt.getImagPartNumeric
    assertEquals(expected, actual, QuadIntTest.TEST_DELTA)
    testRealQuadInt = new RealQuadInt(-3, 1, ringOQ13, 2)
    actual = testRealQuadInt.getImagPartNumeric
    assertEquals(expected, actual, QuadIntTest.TEST_DELTA)
    testRealQuadInt = new RealQuadInt(-5, 2, ringRandom)
    actual = testRealQuadInt.getImagPartNumeric
    assertEquals(expected, actual, QuadIntTest.TEST_DELTA)
  }

  @Test def testCompare(): Unit = {
    println("compare")
    val ring = new RealQuadRing(10)
    val negSeven = new RealQuadInt(-7, 0, ring)
    val numNeg136plus44Sqrt10 = new RealQuadInt(-136, 44, ring)
    val num117minus36Sqrt10 = new RealQuadInt(117, -36, ring)
    val numSqrt10 = new RealQuadInt(0, 1, ring)
    var assertionMessage = negSeven.toString + " should be found to be less than " + numNeg136plus44Sqrt10.toString
    assertTrue(assertionMessage, negSeven < numNeg136plus44Sqrt10)
    assertionMessage = numNeg136plus44Sqrt10.toString + " should be found to be less than " + num117minus36Sqrt10.toString
    assertTrue(assertionMessage, numNeg136plus44Sqrt10 < num117minus36Sqrt10)
    assertionMessage = num117minus36Sqrt10.toString + " should be found to be less than " + numSqrt10.toString
    assertTrue(assertionMessage, num117minus36Sqrt10 < numSqrt10)
    var sameNum = new RealQuadInt(-7, 0, ring)
    var comparison = sameNum.compare(negSeven)
    assertEquals(0, comparison)
    sameNum = new RealQuadInt(-136, 44, ring)
    comparison = sameNum.compare(numNeg136plus44Sqrt10)
    assertEquals(0, comparison)
    sameNum = new RealQuadInt(117, -36, ring)
    comparison = sameNum.compare(num117minus36Sqrt10)
    assertEquals(0, comparison)
    sameNum = new RealQuadInt(0, 1, ring)
    comparison = sameNum.compare(numSqrt10)
    assertEquals(0, comparison)
    assertionMessage = numSqrt10.toString + " should be found to be greater than " + num117minus36Sqrt10.toString
    assertTrue(assertionMessage, numSqrt10 > num117minus36Sqrt10)
    assertionMessage = num117minus36Sqrt10.toString + " should be found to be greater than " + numNeg136plus44Sqrt10.toString
    assertTrue(assertionMessage, num117minus36Sqrt10 > numNeg136plus44Sqrt10)
    assertionMessage = numNeg136plus44Sqrt10.toString + " should be found to be greater than " + negSeven.toString
    assertTrue(assertionMessage, numNeg136plus44Sqrt10 > negSeven)
  }

  @Test def testCompareThroughCollectionSort(): Unit = {
    val ring = new RealQuadRing(10)
    val negSeven = new RealQuadInt(-7, 0, ring)
    val numNeg136plus44Sqrt10 = new RealQuadInt(-136, 44, ring)
    val num117minus36Sqrt10 = new RealQuadInt(117, -36, ring)
    val numSqrt10 = new RealQuadInt(0, 1, ring)
    val scrambledList = List(num117minus36Sqrt10, numSqrt10, negSeven, numNeg136plus44Sqrt10)
    val expected = List(negSeven, numNeg136plus44Sqrt10, num117minus36Sqrt10, numSqrt10)
    val actual = scrambledList.sorted
    assertEquals(expected, actual)
  }

  @Test def testPlus(): Unit = {
    println("+")
    var testAddendA = new RealQuadInt(15, 4, ringZ2)
    var testAddendB = new RealQuadInt(2, 1, ringZ2)
    var expected = new RealQuadInt(17, 5, ringZ2)
    var actual = testAddendA + testAddendB
    assertEquals(expected, actual)
    testAddendA = new RealQuadInt(0, -193, ringZPhi)
    testAddendB = new RealQuadInt(1, 389, ringZPhi, 2)
    expected = new RealQuadInt(1, 3, ringZPhi, 2)
    actual = testAddendA + testAddendB
    assertEquals(expected, actual)
    testAddendA = new RealQuadInt(-43, 9, ringOQ13, 2)
    testAddendB = new RealQuadInt(3, 1, ringOQ13, 2)
    expected = new RealQuadInt(-20, 5, ringOQ13)
    actual = testAddendA + testAddendB
    assertEquals(expected, actual)
    testAddendA = new RealQuadInt(4, 7, ringRandom)
    testAddendB = new RealQuadInt(8, 3, ringRandom)
    expected = new RealQuadInt(12, 10, ringRandom)
    actual = testAddendA + testAddendB
    assertEquals(expected, actual)
  }

  @Test def testPlusAdditiveInverses(): Unit = {
    val expected = new RealQuadInt(0, 0, ringRandom)
    for {a <- -10 to 10
         b <- -10 to 10} {
      val testAddendA = new RealQuadInt(a, b, ringRandom)
      val testAddendB = new RealQuadInt(-a, -b, ringRandom)
      val actual = testAddendA + testAddendB
      assertEquals(expected, actual)
    }
  }

  @Test def testPlusToQuarticDomain(): Unit = {
    val testAddendA = new RealQuadInt(5, 3, ringZ2)
    val testAddendB = new RealQuadInt(7, 2, ringRandom)
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
      val testNumber = new RealQuadInt(a, b, ringRandom)
      val expected = new RealQuadInt(-a, -b, ringRandom)
      val actual = -testNumber
      assertEquals(expected, actual)
    }
  }

  @Test def testMinus(): Unit = {
    println("-")
    var testSubtrahend = new RealQuadInt(15, 4, ringZ2)
    var testMinuend = new RealQuadInt(2, 1, ringZ2)
    var expected = new RealQuadInt(13, 3, ringZ2)
    var actual = testSubtrahend - testMinuend
    assertEquals(expected, actual)
    testSubtrahend = new RealQuadInt(0, -193, ringZPhi)
    testMinuend = new RealQuadInt(1, 389, ringZPhi, 2)
    expected = new RealQuadInt(-1, -775, ringZPhi, 2)
    actual = testSubtrahend - testMinuend
    assertEquals(expected, actual)
    testSubtrahend = new RealQuadInt(-43, 9, ringOQ13, 2)
    testMinuend = new RealQuadInt(3, 1, ringOQ13, 2)
    expected = new RealQuadInt(-23, 4, ringOQ13)
    actual = testSubtrahend - testMinuend
    assertEquals(expected, actual)
    testSubtrahend = new RealQuadInt(4, 7, ringRandom)
    testMinuend = new RealQuadInt(8, 3, ringRandom)
    expected = new RealQuadInt(-4, 4, ringRandom)
    actual = testSubtrahend - testMinuend
    assertEquals(expected, actual)
  }

  @Test def testTimes(): Unit = {
    println("*")
    var testMultiplicandA = new RealQuadInt(15, 4, ringZ2)
    var testMultiplicandB = new RealQuadInt(2, 1, ringZ2)
    var expected = new RealQuadInt(38, 23, ringZ2)
    var actual = testMultiplicandA * testMultiplicandB
    assertEquals(expected, actual)
    testMultiplicandA = new RealQuadInt(0, -193, ringZPhi)
    testMultiplicandB = new RealQuadInt(1, 389, ringZPhi, 2)
    expected = new RealQuadInt(-375385, -193, ringZPhi, 2)
    actual = testMultiplicandA * testMultiplicandB
    assertEquals(expected, actual)
    testMultiplicandA = new RealQuadInt(-43, 9, ringOQ13, 2)
    testMultiplicandB = new RealQuadInt(3, 1, ringOQ13, 2)
    expected = new RealQuadInt(-3, -4, ringOQ13)
    actual = testMultiplicandA * testMultiplicandB
    assertEquals(expected, actual)
    testMultiplicandA = new RealQuadInt(4, 7, ringRandom)
    testMultiplicandB = new RealQuadInt(8, 3, ringRandom)
    val expReg = 32 + 21 * ringRandom.radicand
    expected = new RealQuadInt(expReg, 68, ringRandom)
    actual = testMultiplicandA * testMultiplicandB
    assertEquals(expected, actual)
  }

  @Test def testDivides(): Unit = {
    println("/")
    var testDividend = new RealQuadInt(38, 23, ringZ2)
    var testDivisor = new RealQuadInt(15, 4, ringZ2)
    var expected = new RealQuadInt(2, 1, ringZ2)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case e: Exception => val failMsg = "Trying to divide " + testDividend.toString + " by " + testDivisor.toString + " should not have caused " + e.getClass.getName
        fail(failMsg)
    }
    testDivisor = expected
    expected = new RealQuadInt(15, 4, ringZ2)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case e: Exception => val failMsg = "Trying to divide " + testDividend.toString + " by " + testDivisor.toString + " should not have caused " + e.getClass.getName
        fail(failMsg)
    }
    testDividend = new RealQuadInt(-375385, -193, ringZPhi, 2)
    testDivisor = new RealQuadInt(1, 389, ringZPhi, 2)
    expected = new RealQuadInt(0, -193, ringZPhi)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case e: Exception => val failMsg = "Trying to divide " + testDividend.toString + " by " + testDivisor.toString + " should not have caused " + e.getClass.getName
        fail(failMsg)
    }
    testDivisor = expected
    expected = new RealQuadInt(1, 389, ringZPhi, 2)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case e: Exception => val failMsg = "Trying to divide " + testDividend.toString + " by " + testDivisor.toString + " should not have caused " + e.getClass.getName
        fail(failMsg)
    }
    testDividend = new RealQuadInt(-3, -4, ringOQ13)
    testDivisor = new RealQuadInt(3, 1, ringOQ13, 2)
    expected = new RealQuadInt(-43, 9, ringOQ13, 2)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case e: Exception => val failMsg = "Trying to divide " + testDividend.toString + " by " + testDivisor.toString + " should not have caused " + e.getClass.getName
        fail(failMsg)
    }
    testDivisor = expected
    expected = new RealQuadInt(3, 1, ringOQ13, 2)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case e: Exception => val failMsg = "Trying to divide " + testDividend.toString + " by " + testDivisor.toString + " should not have caused " + e.getClass.getName
        fail(failMsg)
    }
    val expReg = 32 + 21 * ringRandom.radicand
    testDividend = new RealQuadInt(expReg, 68, ringRandom)
    testDivisor = new RealQuadInt(8, 3, ringRandom)
    expected = new RealQuadInt(4, 7, ringRandom)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case e: Exception => val failMsg = "Trying to divide " + testDividend.toString + " by " + testDivisor.toString + " should not have caused " + e.getClass.getName
        fail(failMsg)
    }
    testDivisor = expected
    expected = new RealQuadInt(8, 3, ringRandom)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case e: Exception => val failMsg = "Trying to divide " + testDividend.toString + " by " + testDivisor.toString + " should not have caused " + e.getClass.getName
        fail(failMsg)
    }
    testDivisor = expected
    expected = new RealQuadInt(15, 4, ringZ2)
  }

  @Test def testDivideByNonDivisor(): Unit = {
    for (d <- 2 to 19) {
      if (NumberTheoreticFunctionsCalculator.isSquarefree(d)) {
        val r = new RealQuadRing(d)
        val testDividend = new RealQuadInt(20, 20, r)
        val testNonDivisor = new RealQuadInt(20, 19, r)
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
    val testDividend = new RealQuadInt(4, 7, ringRandom)
    val zero = new RealQuadInt(0, 0, ringRandom)
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

}
