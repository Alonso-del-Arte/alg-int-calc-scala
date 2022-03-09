package algebraics.quartics

import algebraics.IntRing

import org.junit.Test
import org.junit.Assert._

class Zeta8RingTest {

  @Test def testGetMaxAlgebraicDegree(): Unit = {
    println("getMaxAlgebraicDegree")
    assertEquals(4, Zeta8Ring.getMaxAlgebraicDegree)
  }

  @Test def testIsPurelyReal(): Unit = {
    println("isPurelyReal")
    val assertionMessage = "Ring with imaginary and complex numbers should not be said to be purely real"
    assertFalse(assertionMessage, Zeta8Ring.isPurelyReal)
  }

  @Test def testDiscriminant(): Unit = {
    println("discriminant")
    assertEquals(256, Zeta8Ring.discriminant)
  }

  @Test def testToString(): Unit = {
    println("toString")
    assertEquals("O_Q(zeta_8)", Zeta8Ring.toString.replace(" ", ""))
  }

  @Test def testToUnicodeString(): Unit = {
    println("toUnicodeString")
    IntRing.preferenceForBlackboardBold = true
    var expected = "O_\u211A(\u03B6\u2088)"
    var actual = Zeta8Ring.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    IntRing.preferenceForBlackboardBold = false
    expected = "O_Q(\u03B6\u2088)"
    actual = Zeta8Ring.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToTeXString(): Unit = {
    println("toTeXString")
    IntRing.preferenceForBlackboardBold = true
    var expected = "\\mathcalO_{\\mathbbQ(\\zeta_8)}"
    var actual = Zeta8Ring.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    IntRing.preferenceForBlackboardBold = false
    expected = "\\mathcalO_{\\textbfQ(\\zeta_8)}"
    actual = Zeta8Ring.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToHTMLString(): Unit = {
    println("toHTMLString")
    IntRing.preferenceForBlackboardBold = true
    var expected = "<i>O</i><sub>&#x211A;(&zeta;<sub>8</sub>)</sub>"
    var actual = Zeta8Ring.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    IntRing.preferenceForBlackboardBold = false
    expected = "<i>O</i><sub><b>Q</b>(&zeta;<sub>8</sub>)</sub>"
    actual = Zeta8Ring.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToFilenameString(): Unit = {
    println("toFilenameString")
    assertEquals("O_QZETA_8", Zeta8Ring.toFilenameString.replace(" ", ""))
  }

}
