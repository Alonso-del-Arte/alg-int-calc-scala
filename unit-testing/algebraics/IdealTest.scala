package algebraics

import algebraics.quadratics.{ImagQuadInt, ImagQuadRing, RealQuadInt, RealQuadRing}

import org.junit.Test
import org.junit.Assert._

class IdealTest {
  private val ringZi5 = new ImagQuadRing(-5)
  private val ringZ10 = new RealQuadRing(10)
  private val algInt2InZi5 = new ImagQuadInt(2, 0, ringZi5)
  private val algInt1PlusSqrtNeg5 = new ImagQuadInt(1, 1, ringZi5)
  private val algInt2InZ10 = new RealQuadInt(2, 0, ringZ10)
  private val algIntSqrt10 = new RealQuadInt(0, 1, ringZ10)
  private val unit = new RealQuadInt(3, 1, ringZ10)
  private val idealWholeRing = new Ideal(unit)
  private val idealPrincipalZi5 = new Ideal(algInt2InZi5)
  private val idealPrincipalZ10 = new Ideal(algIntSqrt10)
  private val idealSecondaryZi5 = new Ideal(algInt2InZi5, algInt1PlusSqrtNeg5)
  private val idealSecondaryZ10 = new Ideal(algInt2InZ10, algIntSqrt10)

  @Test def testNorm(): Unit = {
    println("norm")
    var expected = 1L
    var actual = idealWholeRing.norm
    assertEquals(expected, actual)
    expected = 2L
    actual = idealSecondaryZi5.norm
    assertEquals(expected, actual)
    actual = idealSecondaryZ10.norm
    assertEquals(expected, actual)
    expected = 4L
    actual = idealPrincipalZi5.norm
    assertEquals(expected, actual)
    expected = 10L
    actual = idealPrincipalZ10.norm
    assertEquals(expected, actual)
  }

  @Test def testIsPrincipal(): Unit = {
    println("isPrincipal")
    var assertionMessage = idealPrincipalZi5.toString + " should be found to be principal"
    assertTrue(assertionMessage, idealPrincipalZi5.isPrincipal)
    assertionMessage = idealPrincipalZ10.toString + " should be found to be principal"
    assertTrue(assertionMessage, idealPrincipalZ10.isPrincipal)
    assertionMessage = idealSecondaryZi5.toString + " should not be found to be principal"
    assertFalse(assertionMessage, idealSecondaryZi5.isPrincipal)
    assertionMessage = idealSecondaryZ10.toString + " should not be found to be principal"
    assertFalse(assertionMessage, idealSecondaryZ10.isPrincipal)
  }

  @Test def testIsMaximal(): Unit = {
    println("isMaximal")
    fail("Haven't written test yet")
  }

  @Test def testContains(): Unit = {
    println("contains")
    fail("Haven't written test yet")
  }

  @Test def testHashCode(): Unit = {
    println("hashCode")
    fail("Haven't written test yet")
  }

  @Test def testEquals(): Unit = {
    println("equals")
    fail("Haven't written test yet")
  }

  @Test def testToString(): Unit = {
    println("toString")
    var expected = "(2)"
    var actual = idealPrincipalZi5.toString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "(sqrt(10))"
    actual = idealPrincipalZ10.toString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "(2,1+sqrt(-5))"
    actual = idealSecondaryZi5.toString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "(2,sqrt(10))"
    actual = idealSecondaryZ10.toString.replace(" ", "")
    assertEquals(expected, actual)
    expected = ringZ10.toString.replace(" ", "")
    actual = idealWholeRing.toString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToUnicodeString(): Unit = {
    println("toUnicodeString")
    var expected = "\u27E82\u27E9"
    var actual = idealPrincipalZi5.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "\u27E8\u221A(10)\u27E9"
    actual = idealPrincipalZ10.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "\u27E82,1+\u221A(\u22125)\u27E9"
    actual = idealSecondaryZi5.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "\u27E82,\u221A(10)\u27E9"
    actual = idealSecondaryZ10.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    expected = ringZ10.toUnicodeString.replace(" ", "")
    actual = idealWholeRing.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToTeXString(): Unit = {
    println("toTeXString")
    var expected = "\\langle2\\rangle"
    var actual = idealPrincipalZi5.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "\\langle\\sqrt{10}\\rangle"
    actual = idealPrincipalZ10.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "\\langle2,1+\\sqrt{-5}\\rangle"
    actual = idealSecondaryZi5.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "\\langle2,\\sqrt{10}\\rangle"
    actual = idealSecondaryZ10.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    expected = ringZ10.toTeXString.replace(" ", "")
    actual = idealWholeRing.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToHTMLString(): Unit = {
    println("toHTMLString")
    var expected = "&#10216;2&#10217;"
    var actual = idealPrincipalZi5.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "&#10216;&radic;(10)&#10217;"
    actual = idealPrincipalZ10.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "&#10216;2,1+&radic;(&minus;5)&#10217;"
    actual = idealSecondaryZi5.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "&#10216;2,&radic;(10)&#10217;"
    actual = idealSecondaryZ10.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    expected = ringZ10.toHTMLString.replace(" ", "")
    actual = idealWholeRing.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
  }

}
