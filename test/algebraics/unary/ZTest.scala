package algebraics.unary

import org.junit.Test
import org.junit.Assert._

class ZTest {

  @Test def testGetMaxAlgebraicDegree(): Unit = {
    println("getMaxAlgebraicDegree")
    assertEquals(1, Z.getMaxAlgebraicDegree)
  }

  @Test def testIsPurelyReal(): Unit = {
    println("isPurelyReal")
    assert(Z.isPurelyReal, "Expecting Z to be purely real")
  }

  @Test def testDiscriminant(): Unit = {
    println("discriminant")
    assertEquals(1, Z.discriminant)
  }

  @Test def testToString(): Unit = {
    println("toString")
    val expected = "Z"
    val actual = Z.toString
    assertEquals(expected, actual)
  }

  @Test def testToUnicodeString(): Unit = {
    println("toUnicodeString")
    val expected = "\u2124"
    val actual = Z.toUnicodeString
    assertEquals(expected, actual)
  }

  @Test def testToTeXString(): Unit = {
    println("toTeXString")
    Z.preferBlackboardBold = true
    var expected = "\\mathbb Z"
    var actual = Z.toTeXString
    assertEquals(expected, actual)
    Z.preferBlackboardBold = false
    expected = "\\textbf Z"
    actual = Z.toTeXString
    assertEquals(expected, actual)
  }

  @Test def testToHTMLString(): Unit = {
    println("toHTMLString")
    Z.preferBlackboardBold = true
    var expected = "\\u2124" // Double-struck capital Z
    var actual = Z.toHTMLString
    assertEquals(expected, actual)
    Z.preferBlackboardBold = false
    expected = "<b>Z</b>"
    actual = Z.toHTMLString
    assertEquals(expected, actual)
  }

  @Test def testToFilenameString(): Unit = {
    println("toFilenameString")
    val expected = "Z"
    val actual = Z.toFilenameString
    assertEquals(expected, actual)
  }

}
