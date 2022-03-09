package algebraics.unary

import org.junit.Test
import org.junit.Assert._

class UnaryIntTest {

  private val TEST_DELTA = 0.00000001

  @Test def testAlgebraicDegree(): Unit = {
    var expected = 1
    var num = new UnaryInt(-7)
    var actual = num.algebraicDegree
    assertEquals(expected, actual)
    num = new UnaryInt(7)
    actual = num.algebraicDegree
    assertEquals(expected, actual)
    expected = 0
    num = new UnaryInt(0)
    actual = num.algebraicDegree
    assertEquals(expected, actual)
  }

  @Test def testTrace(): Unit = {
    var expected = -7
    var num = new UnaryInt(expected)
    var actual = num.trace
    assertEquals(expected, actual)
    expected = 0
    num = new UnaryInt(expected)
    actual = num.trace
    assertEquals(expected, actual)
    expected = 7
    num = new UnaryInt(expected)
    actual = num.trace
    assertEquals(expected, actual)
  }

  @Test def testNorm(): Unit = {
    var expected = -7
    var num = new UnaryInt(expected)
    var actual = num.norm
    assertEquals(expected, actual)
    expected = 0
    num = new UnaryInt(expected)
    actual = num.norm
    assertEquals(expected, actual)
    expected = 7
    num = new UnaryInt(expected)
    actual = num.norm
    assertEquals(expected, actual)
  }

  @Test def testMinPolynomial(): Unit = {
    var expected = Array(7L, 1L)
    var num = new UnaryInt(-7)
    var actual = num.minPolynomialCoeffs
    assertArrayEquals(expected, actual)
    expected = Array(0L, 1L)
    num = new UnaryInt(0)
    actual = num.minPolynomialCoeffs
    assertArrayEquals(expected, actual)
    expected = Array(-7L, 1L)
    num = new UnaryInt(7)
    actual = num.minPolynomialCoeffs
    assertArrayEquals(expected, actual)
  }

  @Test def testMinPolynomialString(): Unit = {
    var expected = "x+7"
    var num = new UnaryInt(-7)
    var actual = num.minPolynomialString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "x"
    num = new UnaryInt(0)
    actual = num.minPolynomialString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "x-7"
    num = new UnaryInt(7)
    actual = num.minPolynomialString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testGetRing(): Unit = {
    val num = new UnaryInt(7)
    assertEquals(Z, num.getRing)
  }

  @Test def testToString(): Unit = {
    var expected = "-7"
    var num = new UnaryInt(-7)
    var actual = num.toString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "0"
    num = new UnaryInt(0)
    actual = num.toString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "7"
    num = new UnaryInt(7)
    actual = num.toString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToUnicodeString(): Unit = {
    var expected = "-7"
    var num = new UnaryInt(-7)
    var actual = num.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "0"
    num = new UnaryInt(0)
    actual = num.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "7"
    num = new UnaryInt(7)
    actual = num.toUnicodeString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToTeXString(): Unit = {
    var expected = "-7"
    var num = new UnaryInt(-7)
    var actual = num.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "0"
    num = new UnaryInt(0)
    actual = num.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "7"
    num = new UnaryInt(7)
    actual = num.toTeXString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToHTMLString(): Unit = {
    var expected = "&minus;7"
    var num = new UnaryInt(-7)
    var actual = num.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "0"
    num = new UnaryInt(0)
    actual = num.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "7"
    num = new UnaryInt(7)
    actual = num.toHTMLString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testAbs(): Unit = {
    var expected = 7.0
    var num = new UnaryInt(-7)
    var actual = num.abs
    assertEquals(expected, actual, TEST_DELTA)
    num = new UnaryInt(7)
    actual = num.abs
    assertEquals(expected, actual, TEST_DELTA)
    expected = 0.0
    num = new UnaryInt(0)
    actual = num.abs
    assertEquals(expected, actual, TEST_DELTA)
  }

  @Test def testAngle(): Unit = {
    var expected = Math.PI
    var num = new UnaryInt(-7)
    var actual = num.angle
    assertEquals(expected, actual, TEST_DELTA)
    expected = 0.0
    num = new UnaryInt(0)
    actual = num.angle
    assertEquals(expected, actual, TEST_DELTA)
    num = new UnaryInt(7)
    actual = num.angle
    assertEquals(expected, actual, TEST_DELTA)
  }

  @Test def testGetRealPartNumeric(): Unit = {
    var expected = -7.0
    var num = new UnaryInt(-7)
    var actual = num.getRealPartNumeric
    assertEquals(expected, actual, TEST_DELTA)
    expected = 0.0
    num = new UnaryInt(0)
    actual = num.getRealPartNumeric
    assertEquals(expected, actual, TEST_DELTA)
    expected = 7.0
    num = new UnaryInt(7)
    actual = num.getRealPartNumeric
    assertEquals(expected, actual, TEST_DELTA)
  }

  @Test def testGetImagPartNumeric(): Unit = {
    val expected = 0.0
    var num = new UnaryInt(-7)
    var actual = num.getImagPartNumeric
    assertEquals(expected, actual, TEST_DELTA)
    num = new UnaryInt(0)
    actual = num.getImagPartNumeric
    assertEquals(expected, actual, TEST_DELTA)
    num = new UnaryInt(7)
    actual = num.getImagPartNumeric
    assertEquals(expected, actual, TEST_DELTA)
  }

}
