package algebraics.quadratics

import algebraics.IntRing
import calculators.NumberTheoreticFunctionsCalculator

import org.junit.Test
import org.junit.Assert._

class ImagQuadRingTest {
  private val ringGaussian = new ImagQuadRing(-1)
  private val ringZi2 = new ImagQuadRing(-2)
  private val ringEisenstein = new ImagQuadRing(-3)
  private val ringOQi7 = new ImagQuadRing(-7)
  private val testParamD = NumberTheoreticFunctionsCalculator.randomSquarefreeNumber(8192) match {
    case 1 => -5
    case 3 => -6
    case n: Int => -n
  }
  private val ringRandom = new ImagQuadRing(testParamD)

  @Test def testConstructor(): Unit = {
    try {
      val badRing = new ImagQuadRing(-12)
      fail("Trying to create " + badRing.toString + " should have caused an exception.")
    } catch {
      case iae: IllegalArgumentException => println("Parameter -12 for constructor correctly caused IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
      case e: Exception => fail(e.getClass.getName + " is not appropriate for parameter -12")
    }
    try {
      val wrongKindRing = new ImagQuadRing(7)
      fail("Trying to create " + wrongKindRing.toString + " should have caused an exception.")
    } catch {
      case iae: IllegalArgumentException => println("Parameter 7 for constructor correctly caused IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
      case e: Exception => fail(e.getClass.getName + " is not appropriate for parameter 7")
    }
  }

  @Test def testGetMaxAlgebraicDegree(): Unit = {
    println("getMaxAlgebraicDegree")
    assertEquals(2, ringGaussian.getMaxAlgebraicDegree)
    assertEquals(2, ringZi2.getMaxAlgebraicDegree)
    assertEquals(2, ringEisenstein.getMaxAlgebraicDegree)
    assertEquals(2, ringOQi7.getMaxAlgebraicDegree)
    assertEquals(2, ringRandom.getMaxAlgebraicDegree)
  }

  @Test def testHashCode(): Unit = {
    println("hashCode")
    var sameRing = new ImagQuadRing(-1)
    var newHash = sameRing.hashCode
    var oldHash = ringGaussian.hashCode
    println(ringGaussian.toString + " hashed as " + oldHash.toString + " and " + newHash.toString)
    assertEquals(oldHash, newHash)
    sameRing = new ImagQuadRing(-2)
    newHash = sameRing.hashCode
    oldHash = ringZi2.hashCode
    println(ringZi2.toString + " hashed as " + oldHash.toString + " and " + newHash.toString)
    assertEquals(oldHash, newHash)
    sameRing = new ImagQuadRing(-3)
    newHash = sameRing.hashCode
    oldHash = ringEisenstein.hashCode
    println(ringEisenstein.toString + " hashed as " + oldHash.toString + " and " + newHash.toString)
    assertEquals(oldHash, newHash)
    sameRing = new ImagQuadRing(-7)
    newHash = sameRing.hashCode
    oldHash = ringOQi7.hashCode
    println(ringOQi7.toString + " hashed as " + oldHash.toString + " and " + newHash.toString)
    assertEquals(oldHash, newHash)
    sameRing = new ImagQuadRing(testParamD)
    newHash = sameRing.hashCode
    oldHash = ringRandom.hashCode
    println(ringRandom.toString + " hashed as " + oldHash.toString + " and " + newHash.toString)
    assertEquals(oldHash, newHash)
  }

  @Test def testEquals(): Unit = {
    println("equals")
    var sameRing = new ImagQuadRing(-1)
    assertEquals(ringGaussian, sameRing)
    sameRing = new ImagQuadRing(-2)
    assertEquals(ringZi2, sameRing)
    sameRing = new ImagQuadRing(-3)
    assertEquals(ringEisenstein, sameRing)
    sameRing = new ImagQuadRing(-7)
    assertEquals(ringOQi7, sameRing)
    sameRing = new ImagQuadRing(testParamD)
    assertEquals(ringRandom, sameRing)
    val diffRing = new ImagQuadRing(-11)
    assertNotEquals(ringGaussian, diffRing)
    assertNotEquals(ringZi2, diffRing)
    assertNotEquals(ringEisenstein, diffRing)
    assertNotEquals(ringOQi7, diffRing)
    if (testParamD == -11) {
      assertNotEquals(ringGaussian, ringRandom)
    } else {
      assertNotEquals(ringRandom, diffRing)
    }
  }

  @Test def testToString(): Unit = {
    println("toString")
    assertEquals("Z[i]", ringGaussian.toString)
    assertEquals("Z[sqrt(-2)]", ringZi2.toString)
    assertEquals("Z[omega]", ringEisenstein.toString)
    assertEquals("O_(Q(sqrt(-7)))", ringOQi7.toString)
    var expected = testParamD.toString
    if (testParamD % 4 == -3) {
      expected = "O_(Q(sqrt(" + expected + ")))"
    } else {
      expected = "Z[sqrt(" + expected + ")]"
    }
    assertEquals(expected, ringRandom.toString)
  }

  @Test def testToUnicodeString(): Unit = {
    println("toUnicodeString")
    IntRing.preferenceForBlackboardBold = true
    assertEquals("\u2124[i]", ringGaussian.toUnicodeString)
    assertEquals("\u2124[\u221A(\u22122)]", ringZi2.toUnicodeString)
    assertEquals("\u2124[\u03C9]", ringEisenstein.toUnicodeString)
    assertEquals("O_(\u211A(\u221A(\u22127)))", ringOQi7.toUnicodeString)
    var expected = (-testParamD).toString
    if (testParamD % 4 == -3) {
      expected = "O_(\u211A(\u221A(\u2212" + expected + ")))"
    } else {
      expected = "\u2124[\u221A(\u2212" + expected + ")]"
    }
    assertEquals(expected, ringRandom.toUnicodeString)
    IntRing.preferenceForBlackboardBold = false
    assertEquals("Z[i]", ringGaussian.toUnicodeString)
    assertEquals("Z[\u221A(\u22122)]", ringZi2.toUnicodeString)
    assertEquals("Z[\u03C9]", ringEisenstein.toUnicodeString)
    assertEquals("O_(Q(\u221A(\u22127)))", ringOQi7.toUnicodeString)
    expected = (-testParamD).toString
    if (testParamD % 4 == -3) {
      expected = "O_(Q(\u221A(\u2212" + expected + ")))"
    } else {
      expected = "Z[\u221A(\u2212" + expected + ")]"
    }
    assertEquals(expected, ringRandom.toUnicodeString)
  }

  @Test def testToTeXString(): Unit = {
    println("toTeXString")
    IntRing.preferenceForBlackboardBold = true
    assertEquals("\\mathbb Z[i]", ringGaussian.toTeXString)
    assertEquals("\\mathbb Z[\\sqrt{-2}]", ringZi2.toTeXString)
    assertEquals("\\mathbb Z[\\omega]", ringEisenstein.toTeXString)
    assertEquals("\\mathcal O_{\\mathbb Q(\\sqrt{-7})}", ringOQi7.toTeXString)
    if (testParamD % 4 == -3) {
      assertEquals("\\mathcal O_{\\mathbb Q(\\sqrt{" + testParamD.toString + "})}", ringRandom.toTeXString)
    } else {
      assertEquals("\\mathbb Z[\\sqrt{" + testParamD.toString + "}]", ringRandom.toTeXString)
    }
    IntRing.preferenceForBlackboardBold = false
    assertEquals("\\textbf Z[i]", ringGaussian.toTeXString)
    assertEquals("\\textbf Z[\\sqrt{-2}]", ringZi2.toTeXString)
    assertEquals("\\textbf Z[\\omega]", ringEisenstein.toTeXString)
    assertEquals("\\mathcal O_{\\textbf Q(\\sqrt{-7})}", ringOQi7.toTeXString)
    if (testParamD % 4 == -3) {
      assertEquals("\\mathcal O_{\\textbf Q(\\sqrt{" + testParamD.toString + "})}", ringRandom.toTeXString)
    } else {
      assertEquals("\\textbf Z[\\sqrt{" + testParamD.toString + "}]", ringRandom.toTeXString)
    }
  }

  @Test def testToHTMLString(): Unit = {
    println("toHTMLString")
    IntRing.preferenceForBlackboardBold = true
    assertEquals("&#x2124;[<i>i</i>]", ringGaussian.toHTMLString)
    assertEquals("&#x2124;[&radic;(&minus;2)]", ringZi2.toHTMLString)
    assertEquals("&#x2124;[&omega;]", ringEisenstein.toHTMLString)
    assertEquals("<i>O</i><sub>&#x211A;(&radic;(&minus;7))</sub>", ringOQi7.toHTMLString)
    var expected = (-testParamD).toString
    if (testParamD % 4 == -3) {
      expected = "<i>O</i><sub>&#x211A;(&radic;(&minus;" + expected + "))</sub>"
    } else {
      expected = "&#x2124;[&radic;(&minus;" + expected + ")]"
    }
    assertEquals(expected, ringRandom.toHTMLString)
    IntRing.preferenceForBlackboardBold = false
    assertEquals("<b>Z</b>[<i>i</i>]", ringGaussian.toHTMLString)
    assertEquals("<b>Z</b>[&radic;(&minus;2)]", ringZi2.toHTMLString)
    assertEquals("<b>Z</b>[&omega;]", ringEisenstein.toHTMLString)
    assertEquals("<i>O</i><sub><b>Q</b>(&radic;(&minus;7))</sub>", ringOQi7.toHTMLString)
    expected = (-testParamD).toString
    if (testParamD % 4 == -3) {
      expected = "<i>O</i><sub><b>Q</b>(&radic;(&minus;" + expected + "))</sub>"
    } else {
      expected = "<b>Z</b>[&radic;(&minus;" + expected + ")]"
    }
    assertEquals(expected, ringRandom.toHTMLString)
  }

  @Test def testToFilenameString(): Unit = {
    println("toFilenameString")
    assertEquals("ZI", ringGaussian.toFilenameString)
    assertEquals("ZI2", ringZi2.toFilenameString)
    assertEquals("ZW", ringEisenstein.toFilenameString)
    assertEquals("OQI7", ringOQi7.toFilenameString)
    var expected = (-testParamD).toString
    if (testParamD % 4 == -3) {
      expected = "OQI" + expected
    } else {
      expected = "ZI" + expected
    }
    assertEquals(expected, ringRandom.toFilenameString)
  }

  @Test def testHasHalfIntegers(): Unit = {
    println("hasHalfIntegers")
    var assertionMessage = ringGaussian.toString + " should not be said to have \"half-integers\""
    assertFalse(assertionMessage, ringGaussian.hasHalfIntegers)
    assertionMessage = ringZi2.toString + " should not be said to have \"half-integers\""
    assertFalse(assertionMessage, ringZi2.hasHalfIntegers)
    assertionMessage = ringEisenstein.toString + " should be said to have \"half-integers\""
    assertTrue(assertionMessage, ringEisenstein.hasHalfIntegers)
    assertionMessage = ringOQi7.toString + " should be said to have \"half-integers\""
    assertTrue(assertionMessage, ringOQi7.hasHalfIntegers)
    if (testParamD % 4 == -3) {
      assertionMessage = ringRandom.toString + " should be said to have \"half-integers\""
      assertTrue(assertionMessage, ringRandom.hasHalfIntegers)
    } else {
      assertionMessage = ringRandom.toString + " should not be said to have \"half-integers\""
      assertFalse(assertionMessage, ringRandom.hasHalfIntegers)
    }
  }

  @Test def testIsPurelyReal(): Unit = {
    println("isPurelyReal")
    val assertionMessage = "Imaginary quadratic ring should not be found to be purely real"
    assertFalse(assertionMessage, ringGaussian.isPurelyReal)
    assertFalse(assertionMessage, ringZi2.isPurelyReal)
    assertFalse(assertionMessage, ringEisenstein.isPurelyReal)
    assertFalse(assertionMessage, ringOQi7.isPurelyReal)
    assertFalse(assertionMessage, ringRandom.isPurelyReal)
  }

  @Test def testDiscriminant(): Unit = {
    println("discriminant")
    assertEquals(-4, ringGaussian.discriminant)
    assertEquals(-8, ringZi2.discriminant)
    assertEquals(-3, ringEisenstein.discriminant)
    assertEquals(-7, ringOQi7.discriminant)
    val expected = testParamD * (if (testParamD % 4 == -3) 1 else 4)
    assertEquals(expected, ringRandom.discriminant)
  }

  @Test def testGetRadSqrt(): Unit = {
    println("getRadSqrt")
    val rings = Vector(ringGaussian, ringZi2, ringEisenstein, ringOQi7, ringRandom)
    for (ring <- rings) {
      try {
        val result = ring.getRadSqrt
        val failMessage = "Trying to use getRadSqrt on imaginary ring " + ring.toString + " should have triggered an exception, not given result " + result.toString
        fail(failMessage)
      } catch {
        case uoe: UnsupportedOperationException => println("Trying to use getRadSqrt on imaginary ring " + ring.toString + " correctly triggered UnsupportedOperationException")
          println(uoe.getMessage)
        case e: Exception => val failMessage = e.getClass.getName + " is wrong exception for trying to use getRadSqrt on imaginary ring " + ring.toString
          fail(failMessage)
      }
    }
  }

  @Test def testGetAbsNegRad(): Unit = {
    println("getAbsNegRad")
    assertEquals(1, ringGaussian.getAbsNegRad)
    assertEquals(2, ringZi2.getAbsNegRad)
    assertEquals(3, ringEisenstein.getAbsNegRad)
    assertEquals(7, ringOQi7.getAbsNegRad)
    assertEquals(-testParamD, ringRandom.getAbsNegRad)
  }

  @Test def testGetAbsNegRadSqrt(): Unit = {
    println("getAbsNegRadSqrt")
    val testDelta = 0.00000001
    assertEquals(1.0, ringGaussian.getAbsNegRadSqrt, testDelta)
    assertEquals(Math.sqrt(2), ringZi2.getAbsNegRadSqrt, testDelta)
    assertEquals(Math.sqrt(3), ringEisenstein.getAbsNegRadSqrt, testDelta)
    assertEquals(Math.sqrt(7), ringOQi7.getAbsNegRadSqrt, testDelta)
    assertEquals(Math.sqrt(-testParamD), ringRandom.getAbsNegRadSqrt, testDelta)
  }

}
