package algebraics.quadratics

import algebraics.IntRing
import calculators.NumberTheoreticFunctionsCalculator
import org.junit.Test
import org.junit.Assert._

class RealQuadRingTest {
  private val ringZ2 = new RealQuadRing(2)
  private val ringZPhi = new RealQuadRing(5)
  private val ringOQ13 = new RealQuadRing(13)
  private val testParamD: Int = NumberTheoreticFunctionsCalculator.randomSquarefreeNumber(128) match {
    case 1 => 3
    case 5 => 6
    case n: Int => n
  }
  private val ringRandom = new RealQuadRing(testParamD)

  @Test def testConstructor(): Unit = {
    try {
      val badRing = new RealQuadRing(12)
      fail("Trying to create " + badRing.toString + " should have caused an exception.")
    } catch {
      case iae: IllegalArgumentException => println("Parameter 12 for constructor correctly caused IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
      case e: Exception => fail(e.getClass.getName + " is not appropriate for parameter 12")
    }
    try {
      val wrongKindRing = new RealQuadRing(-7)
      fail("Trying to create " + wrongKindRing.toString + " should have caused an exception.")
    } catch {
      case iae: IllegalArgumentException => println("Parameter -7 for constructor correctly caused IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
      case e: Exception => fail(e.getClass.getName + " is not appropriate for parameter -7")
    }
  }

  @Test def testGetMaxAlgebraicDegree(): Unit = {
    println("getMaxAlgebraicDegree")
    assertEquals(2, ringZ2.getMaxAlgebraicDegree)
    assertEquals(2, ringZPhi.getMaxAlgebraicDegree)
    assertEquals(2, ringOQ13.getMaxAlgebraicDegree)
    assertEquals(2, ringRandom.getMaxAlgebraicDegree)
  }

  @Test def testHashCode(): Unit = {
    println("hashCode")
    var sameRing = new RealQuadRing(2)
    var newHash = sameRing.hashCode
    var oldHash = ringZ2.hashCode
    println(ringZ2.toString + " hashed as " + oldHash.toString + " and " + newHash.toString)
    assertEquals(oldHash, newHash)
    sameRing = new RealQuadRing(5)
    newHash = sameRing.hashCode
    oldHash = ringZPhi.hashCode
    println(ringZPhi.toString + " hashed as " + oldHash.toString + " and " + newHash.toString)
    assertEquals(oldHash, newHash)
    sameRing = new RealQuadRing(13)
    newHash = sameRing.hashCode
    oldHash = ringOQ13.hashCode
    println(ringOQ13.toString + " hashed as " + oldHash.toString + " and " + newHash.toString)
    assertEquals(oldHash, newHash)
    sameRing = new RealQuadRing(testParamD)
    newHash = sameRing.hashCode
    oldHash = ringRandom.hashCode
    println(ringRandom.toString + " hashed as " + oldHash.toString + " and " + newHash.toString)
    assertEquals(oldHash, newHash)
  }

  @Test def testEquals(): Unit = {
    println("equals")
    var sameRing = new RealQuadRing(2)
    assertEquals(ringZ2, sameRing)
    sameRing = new RealQuadRing(5)
    assertEquals(ringZPhi, sameRing)
    sameRing = new RealQuadRing(13)
    assertEquals(ringOQ13, sameRing)
    sameRing = new RealQuadRing(testParamD)
    assertEquals(ringRandom, sameRing)
    val diffRing = new RealQuadRing(3)
    assertNotEquals(ringZ2, diffRing)
    assertNotEquals(ringZPhi, diffRing)
    assertNotEquals(ringOQ13, diffRing)
    if (testParamD == 3) {
      assertNotEquals(ringZ2, ringRandom)
    } else {
      assertNotEquals(ringRandom, diffRing)
    }
  }

  @Test def testToString(): Unit = {
    println("toString")
    assertEquals("Z[sqrt(2)]", ringZ2.toString)
    assertEquals("Z[phi]", ringZPhi.toString)
    assertEquals("O_(Q(sqrt(13)))", ringOQ13.toString)
    if (testParamD % 4 == 1) {
      assertEquals("O_(Q(sqrt(" + testParamD.toString + ")))", ringRandom.toString)
    } else {
      assertEquals("Z[sqrt(" + testParamD.toString + ")]", ringRandom.toString)
    }
  }

  @Test def testToUnicodeString(): Unit = {
    println("toUnicodeString")
    IntRing.preferenceForBlackboardBold = true
    assertEquals("\u2124[\u221A(2)]", ringZ2.toUnicodeString)
    assertEquals("\u2124[\u03C6]", ringZPhi.toUnicodeString)
    assertEquals("O_(\u211A(\u221A(13)))", ringOQ13.toUnicodeString)
    if (testParamD % 4 == 1) {
      assertEquals("O_(\u211A(\u221A(" + testParamD.toString + ")))", ringRandom.toUnicodeString)
    } else {
      assertEquals("\u2124[\u221A(" + testParamD.toString + ")]", ringRandom.toUnicodeString)
    }
    IntRing.preferenceForBlackboardBold = false
    assertEquals("Z[\u221A(2)]", ringZ2.toUnicodeString)
    assertEquals("Z[\u03C6]", ringZPhi.toUnicodeString)
    assertEquals("O_(Q(\u221A(13)))", ringOQ13.toUnicodeString)
    if (testParamD % 4 == 1) {
      assertEquals("O_(Q(\u221A(" + testParamD.toString + ")))", ringRandom.toUnicodeString)
    } else {
      assertEquals("Z[\u221A(" + testParamD.toString + ")]", ringRandom.toUnicodeString)
    }
  }

  @Test def testToTeXString(): Unit = {
    println("toTeXString")
    IntRing.preferenceForBlackboardBold = true
    assertEquals("\\mathbb Z[\\sqrt{2}]", ringZ2.toTeXString)
    assertEquals("\\mathbb Z[\\phi]", ringZPhi.toTeXString)
    assertEquals("\\mathcal O_{\\mathbb Q(\\sqrt{13})}", ringOQ13.toTeXString)
    if (testParamD % 4 == 1) {
      assertEquals("\\mathcal O_{\\mathbb Q(\\sqrt{" + testParamD.toString + "})}", ringRandom.toTeXString)
    } else {
      assertEquals("\\mathbb Z[\\sqrt{" + testParamD.toString + "}]", ringRandom.toTeXString)
    }
    IntRing.preferenceForBlackboardBold = false
    assertEquals("\\textbf Z[\\sqrt{2}]", ringZ2.toTeXString)
    assertEquals("\\textbf Z[\\phi]", ringZPhi.toTeXString)
    assertEquals("\\mathcal O_{\\textbf Q(\\sqrt{13})}", ringOQ13.toTeXString)
    if (testParamD % 4 == 1) {
      assertEquals("\\mathcal O_{\\textbf Q(\\sqrt{" + testParamD.toString + "})}", ringRandom.toTeXString)
    } else {
      assertEquals("\\textbf Z[\\sqrt{" + testParamD.toString + "}]", ringRandom.toTeXString)
    }
  }

  @Test def testToHTMLString(): Unit = {
    println("toHTMLString")
    IntRing.preferenceForBlackboardBold = true
    assertEquals("&#x2124;[&radic;(2)]", ringZ2.toHTMLString)
    assertEquals("&#x2124;[&phi;]", ringZPhi.toHTMLString)
    assertEquals("<i>O</i><sub>&#x211A;(&radic;(13))</sub>", ringOQ13.toHTMLString)
    if (testParamD % 4 == 1) {
      assertEquals("<i>O</i><sub>&#x211A;(&radic;(" + testParamD.toString + "))</sub>", ringRandom.toHTMLString)
    } else {
      assertEquals("&#x2124;[&radic;(" + testParamD.toString + ")]", ringRandom.toHTMLString)
    }
    IntRing.preferenceForBlackboardBold = false
    assertEquals("<b>Z</b>[&radic;(2)]", ringZ2.toHTMLString)
    assertEquals("<b>Z</b>[&phi;]", ringZPhi.toHTMLString)
    assertEquals("<i>O</i><sub><b>Q</b>(&radic;(13))</sub>", ringOQ13.toHTMLString)
    if (testParamD % 4 == 1) {
      assertEquals("<i>O</i><sub><b>Q</b>(&radic;(" + testParamD.toString + "))</sub>", ringRandom.toHTMLString)
    } else {
      assertEquals("<b>Z</b>[&radic;(" + testParamD.toString + ")]", ringRandom.toHTMLString)
    }
  }

  @Test def testToFilenameString(): Unit = {
    println("toFilenameString")
    assertEquals("Z2", ringZ2.toFilenameString)
    assertEquals("ZPHI", ringZPhi.toFilenameString)
    assertEquals("OQ13", ringOQ13.toFilenameString)
    if (testParamD % 4 == 1) {
      assertEquals("OQ" + testParamD.toString, ringRandom.toFilenameString)
    } else {
      assertEquals("Z" + testParamD.toString, ringRandom.toFilenameString)
    }
  }

  @Test def testHasHalfIntegers(): Unit = {
    println("hasHalfIntegers")
    var assertionMessage = ringZ2.toString + " should not be said to have \"half-integers\""
    assertFalse(assertionMessage, ringZ2.hasHalfIntegers)
    assertionMessage = ringZPhi.toString + " should be said to have \"half-integers\""
    assertTrue(assertionMessage, ringZPhi.hasHalfIntegers)
    assertionMessage = ringOQ13.toString + " should be said to have \"half-integers\""
    assertTrue(assertionMessage, ringOQ13.hasHalfIntegers)
    if (testParamD % 4 == 1) {
      assertionMessage = ringRandom.toString + " should be said to have \"half-integers\""
      assertTrue(assertionMessage, ringRandom.hasHalfIntegers)
    } else {
      assertionMessage = ringRandom.toString + " should not be said to have \"half-integers\""
      assertFalse(assertionMessage, ringRandom.hasHalfIntegers)
    }
  }

  @Test def testIsPurelyReal(): Unit = {
    println("isPurelyReal")
    val assertionMessage = "Real quadratic ring should be found to be purely real"
    assertTrue(assertionMessage, ringZ2.isPurelyReal)
    assertTrue(assertionMessage, ringZPhi.isPurelyReal)
    assertTrue(assertionMessage, ringOQ13.isPurelyReal)
    assertTrue(assertionMessage, ringRandom.isPurelyReal)
  }

  @Test def testDiscriminant(): Unit = {
    println("discriminant")
    assertEquals(8, ringZ2.discriminant)
    assertEquals(5, ringZPhi.discriminant)
    assertEquals(13, ringOQ13.discriminant)
    val expected = testParamD * (if (testParamD % 4 == 1) 1 else 4)
    assertEquals(expected, ringRandom.discriminant)
  }

  @Test def testGetRadSqrt(): Unit = {
    println("getRadSqrt")
    val testDelta = 0.00000001
    assertEquals(Math.sqrt(2), ringZ2.getRadSqrt, testDelta)
    assertEquals(Math.sqrt(5), ringZPhi.getRadSqrt, testDelta)
    assertEquals(Math.sqrt(13), ringOQ13.getRadSqrt, testDelta)
    assertEquals(Math.sqrt(testParamD), ringRandom.getRadSqrt, testDelta)
  }

  @Test def testGetAbsNegRad(): Unit = {
    println("getAbsNegRad")
    assertEquals(2, ringZ2.getAbsNegRad)
    assertEquals(5, ringZPhi.getAbsNegRad)
    assertEquals(13, ringOQ13.getAbsNegRad)
    assertEquals(testParamD, ringRandom.getAbsNegRad)
  }

  @Test def testGetAbsNegRadSqrt(): Unit = {
    println("getAbsNegRadSqrt")
    val testDelta = 0.00000001
    assertEquals(Math.sqrt(2), ringZ2.getAbsNegRadSqrt, testDelta)
    assertEquals(Math.sqrt(5), ringZPhi.getAbsNegRadSqrt, testDelta)
    assertEquals(Math.sqrt(13), ringOQ13.getAbsNegRadSqrt, testDelta)
    assertEquals(Math.sqrt(testParamD), ringRandom.getAbsNegRadSqrt, testDelta)
  }

}

