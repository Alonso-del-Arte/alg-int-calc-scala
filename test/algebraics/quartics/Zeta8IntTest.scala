package algebraics.quartics

import algebraics.{AlgebraicDegreeOverflowException, NotDivisibleException,
  UnsupportedNumberDomainException}
import algebraics.quadratics.{ImagQuadInt, ImagQuadRing, QuadInt, RealQuadInt,
  RealQuadRing}

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

class Zeta8IntTest {
  private val ZERO = new Zeta8Int(0, 0, 0, 0)
  private val ONE = new Zeta8Int(1, 0, 0, 0)
  private val NEG_ONE = new Zeta8Int(-1, 0, 0, 0)
  private val ZETA_8 = new Zeta8Int(0, 1, 0, 0)
  private val ZETA_8_CUBED = new Zeta8Int(0, 0, 0, 1)
  private val IMAG_UNIT_I = new Zeta8Int(0, 0, 1, 0)
  private val NEG_IMAG_UNIT_I = new Zeta8Int(0, 0, -1, 0)
  private val SQRT_2 = new Zeta8Int(0, 1, 0, -1)
  private val SQRT_NEG_2 = new Zeta8Int(0, 1, 0, 1)

  @Test def testImplicitConversions(): Unit = {
    println("IntToZeta8Int")
    var expected = new Zeta8Int(1, 0, 1, 0)
    var actual = 1 + IMAG_UNIT_I
    assertEquals(expected, actual)
    actual = ONE - 2
    assertEquals(NEG_ONE, actual)
    actual = (-1) * IMAG_UNIT_I
    assertEquals(NEG_IMAG_UNIT_I, actual)
    actual = 2 / SQRT_2
    assertEquals(SQRT_2, actual)
    expected = new Zeta8Int(0, -1, 0, -1)
    actual = 2 / SQRT_NEG_2
    assertEquals(expected, actual)
  }

  @Test def testAlgebraicDegree(): Unit = {
    println("algebraicDegree")
    var expected: Int = 4
    var actual = ZETA_8.algebraicDegree
    assertEquals(expected, actual)
    actual = ZETA_8_CUBED.algebraicDegree
    assertEquals(expected, actual)
    var num = new Zeta8Int(0, 1, 1, 0)
    actual = num.algebraicDegree
    assertEquals(expected, actual)
    expected = 2
    actual = IMAG_UNIT_I.algebraicDegree
    assertEquals(expected, actual)
    actual = NEG_IMAG_UNIT_I.algebraicDegree
    assertEquals(expected, actual)
    actual = SQRT_2.algebraicDegree
    assertEquals(expected, actual)
    actual = SQRT_NEG_2.algebraicDegree
    assertEquals(expected, actual)
    num = new Zeta8Int(0, 8, 0, 8)
    actual = num.algebraicDegree
    assertEquals(expected, actual)
    num = new Zeta8Int(0, 8, 0, -8)
    actual = num.algebraicDegree
    assertEquals(expected, actual)
    expected = 1
    actual = ONE.algebraicDegree
    assertEquals(expected, actual)
    actual = NEG_ONE.algebraicDegree
    assertEquals(expected, actual)
    num = new Zeta8Int(7, 0, 0, 0)
    actual = num.algebraicDegree
    assertEquals(expected, actual)
    expected = 0
    actual = ZERO.algebraicDegree
    assertEquals(expected, actual)
  }

  @Test def testTrace(): Unit = {
    println("trace")
    fail("Haven't written test yet")
  }

  @Test def testNorm(): Unit = {
    println("norm")
    var expected = 1L
    var actual = ZETA_8.norm
    assertEquals(expected, actual)
    actual = ZETA_8_CUBED.norm
    assertEquals(expected, actual)
    actual = IMAG_UNIT_I.norm
    assertEquals(expected, actual)
    actual = NEG_IMAG_UNIT_I.norm
    assertEquals(expected, actual)
    expected = 4L
    actual = SQRT_2.norm
    assertEquals(expected, actual)
    actual = SQRT_NEG_2.norm
    assertEquals(expected, actual)
  }

  @Test def testMinPolynomialCoeffs(): Unit = {
    println("minPolynomialCoeffs")
    fail("Haven't written test yet")
  }

  @Test def testMinPolynomialString(): Unit = {
    println("minPolynomialString")
    var expected = "x^4+1"
    var actual = ZETA_8.minPolynomialString.replace(" ", "")
    assertEquals(expected, actual)
    actual = ZETA_8_CUBED.minPolynomialString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "x^2+1"
    actual = IMAG_UNIT_I.minPolynomialString
    assertEquals(expected, actual)
    actual = NEG_IMAG_UNIT_I.minPolynomialString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "x^2-2"
    actual = SQRT_2.minPolynomialString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "x^2+2"
    actual = SQRT_NEG_2.minPolynomialString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testComplexConjugate(): Unit = {
    println("complexConjugate")
    assertEquals(SQRT_2, SQRT_2.complexConjugate)
    var actual = IMAG_UNIT_I.complexConjugate
    assertEquals(NEG_IMAG_UNIT_I, actual)
    actual = NEG_IMAG_UNIT_I.complexConjugate
    assertEquals(IMAG_UNIT_I, actual)
    var expResult = new Zeta8Int(0, -1, 0, -1)
    actual = SQRT_NEG_2.complexConjugate
    assertEquals(expResult, actual)
    expResult = new Zeta8Int(0, 0, 0, -1)
    actual = ZETA_8.complexConjugate
    assertEquals(expResult, actual)
    expResult = new Zeta8Int(0, -1, 0, 0)
    actual = ZETA_8_CUBED.complexConjugate
    assertEquals(expResult, actual)
  }

  @Test def testGetRing(): Unit = {
    println("getRing")
    assertEquals(Zeta8Ring, ZERO.getRing)
    assertEquals(Zeta8Ring, ONE.getRing)
    assertEquals(Zeta8Ring, NEG_ONE.getRing)
    assertEquals(Zeta8Ring, ZETA_8.getRing)
    assertEquals(Zeta8Ring, ZETA_8_CUBED.getRing)
    assertEquals(Zeta8Ring, IMAG_UNIT_I.getRing)
    assertEquals(Zeta8Ring, NEG_IMAG_UNIT_I.getRing)
    assertEquals(Zeta8Ring, SQRT_2.getRing)
    assertEquals(Zeta8Ring, SQRT_NEG_2.getRing)
  }

  @Test def testToString(): Unit = {
    println("toString")
    var expected = "zeta_8"
    var actual = ZETA_8.toString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "(zeta_8)^3"
    actual = ZETA_8_CUBED.toString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "i"
    actual = IMAG_UNIT_I.toString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "-i"
    actual = NEG_IMAG_UNIT_I.toString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "1"
    actual = ONE.toString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "0"
    actual = ZERO.toString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "-1"
    actual = NEG_ONE.toString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "zeta_8-(zeta_8)^3"
    actual = SQRT_2.toString.replace(" ", "")
    assertEquals(expected, actual)
    expected = "zeta_8+(zeta_8)^3"
    actual = SQRT_NEG_2.toString.replace(" ", "")
    assertEquals(expected, actual)
    var num: Zeta8Int = new Zeta8Int(4, -3, 2, -1)
    expected = "4-3zeta_8+2i-(zeta_8)^3"
    actual = num.toString.replace(" ", "")
    assertEquals(expected, actual)
    num = new Zeta8Int(-5, 0, -21, 3)
    expected = "-5-21i+3(zeta_8)^3"
    actual = num.toString.replace(" ", "")
    assertEquals(expected, actual)
    num = new Zeta8Int(0, 2, 0, 0)
    expected = "2zeta_8"
    actual = num.toString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToUnicodeString(): Unit = {
    println("toUnicodeString")
    fail("Haven't written test yet")
  }

  @Test def testToTeXString(): Unit = {
    println("toTeXString")
    fail("Haven't written test yet")
  }

  @Test def testToHTMLString(): Unit = {
    println("toHTMLString")
    fail("Haven't written test yet")
  }

  @Test def testEquals(): Unit = {
    println("equals")
    var sameNum = new Zeta8Int(0, 1, 0, 0)
    var diffNum = new Zeta8Int(1, 0, 0, 0)
    assertEquals(sameNum, sameNum)
    assertNotEquals(sameNum, diffNum)
    assertEquals(ZETA_8, ZETA_8)
    assertEquals(ZETA_8, sameNum)
    assertNotEquals(ZETA_8, diffNum)
    sameNum = new Zeta8Int(0, 0, 0, 1)
    diffNum = new Zeta8Int(0, 0, 7, -4)
    assertEquals(sameNum, sameNum)
    assertNotEquals(sameNum, diffNum)
    assertEquals(ZETA_8_CUBED, ZETA_8_CUBED)
    assertEquals(ZETA_8_CUBED, sameNum)
    assertNotEquals(ZETA_8_CUBED, diffNum)
    sameNum = new Zeta8Int(0, 0, 1, 0)
    diffNum = new Zeta8Int(0, 2, 0, 0)
    assertEquals(sameNum, sameNum)
    assertNotEquals(sameNum, diffNum)
    assertEquals(IMAG_UNIT_I, IMAG_UNIT_I)
    assertEquals(IMAG_UNIT_I, sameNum)
    assertNotEquals(IMAG_UNIT_I, diffNum)
    sameNum = new Zeta8Int(0, 0, -1, 0)
    diffNum = new Zeta8Int(0, -2, 0, 0)
    assertEquals(sameNum, sameNum)
    assertNotEquals(sameNum, diffNum)
    assertEquals(NEG_IMAG_UNIT_I, NEG_IMAG_UNIT_I)
    assertEquals(NEG_IMAG_UNIT_I, sameNum)
    assertNotEquals(NEG_IMAG_UNIT_I, diffNum)
    sameNum = new Zeta8Int(0, 1, 0, -1)
    diffNum = new Zeta8Int(0, 0, 3, 0)
    assertEquals(sameNum, sameNum)
    assertNotEquals(sameNum, diffNum)
    assertEquals(SQRT_2, SQRT_2)
    assertEquals(SQRT_2, sameNum)
    assertNotEquals(SQRT_2, diffNum)
    sameNum = new Zeta8Int(0, 1, 0, 1)
    diffNum = new Zeta8Int(0, 0, -3, 0)
    assertEquals(sameNum, sameNum)
    assertNotEquals(sameNum, diffNum)
    assertEquals(SQRT_NEG_2, SQRT_NEG_2)
    assertEquals(SQRT_NEG_2, sameNum)
    assertNotEquals(SQRT_NEG_2, diffNum)
  }

  @Test def testAbs(): Unit = {
    println("abs")
    fail("Haven't written test yet")
  }

  @Test def testAngle(): Unit = {
    println("angle")
    fail("Haven't written test yet")
  }

  @Test def testGetRealPartNumeric(): Unit = {
    println("getRealPartNumeric")
    fail("Haven't written test yet")
  }

  @Test def testGetImagPartNumeric(): Unit = {
    println("getImagPartNumeric")
    fail("Haven't written test yet")
  }

  // TODO: Break up into smaller tests
  @Test def testConvertFromQuadInt(): Unit = {
    println("convertFromQuadInt")
    var currImagRing = new ImagQuadRing(-1)
    var currQuadInt: QuadInt = new ImagQuadInt(0, 1, currImagRing)
    var actual = Zeta8Int.convertFromQuadInt(currQuadInt)
    assertEquals(IMAG_UNIT_I, actual)
    currQuadInt = new ImagQuadInt(0, -1, currImagRing)
    actual = Zeta8Int.convertFromQuadInt(currQuadInt)
    assertEquals(NEG_IMAG_UNIT_I, actual)
    currImagRing = new ImagQuadRing(-2)
    currQuadInt = new ImagQuadInt(0, 1, currImagRing)
    actual = Zeta8Int.convertFromQuadInt(currQuadInt)
    assertEquals(SQRT_NEG_2, actual)
    var currRealRing = new RealQuadRing(2)
    currQuadInt = new RealQuadInt(0, 1, currRealRing)
    actual = Zeta8Int.convertFromQuadInt(currQuadInt)
    assertEquals(SQRT_2, actual)
    currRealRing = new RealQuadRing(3)
    currQuadInt = new RealQuadInt(7, 0, currRealRing)
    val expected = new Zeta8Int(7, 0, 0, 0)
    actual = Zeta8Int.convertFromQuadInt(currQuadInt)
    assertEquals(expected, actual)
    currQuadInt = new RealQuadInt(8, 3, currRealRing)
    val exc = assertThrows(classOf[UnsupportedNumberDomainException], () => {
      actual = Zeta8Int.convertFromQuadInt(currQuadInt)
      println("Trying to convert " + currQuadInt.toString + " to an integer in "
        + Zeta8Ring.toString + " should not have given result "
        + actual.toString)
    })
    val excMsg = exc.getMessage
    assert(excMsg != null, "Message should not be null")
    println("\"" + excMsg + "\"")
  }

  // TODO: Break up into smaller tests
  @Test def testConvertToQuadInt(): Unit = {
    println("convertToQuadInt")
    var currImagRing = new ImagQuadRing(-2)
    var expected: QuadInt = new ImagQuadInt(0, 1, currImagRing)
    var actual = SQRT_NEG_2.convertToQuadInt
    assertEquals(expected, actual)
    currImagRing = new ImagQuadRing(-1)
    expected = new ImagQuadInt(0, 1, currImagRing)
    actual = IMAG_UNIT_I.convertToQuadInt
    assertEquals(expected, actual)
    val ringZ2 = new RealQuadRing(2)
    expected = new RealQuadInt(0, 1, ringZ2)
    actual = SQRT_2.convertToQuadInt
    assertEquals(expected, actual)
    val exc = assertThrows(classOf[AlgebraicDegreeOverflowException], () => {
      actual = ZETA_8.convertToQuadInt
      println("Trying to convert " + ZETA_8.toString
        + " to QuadInt should not have given result " + actual.toString)
    })
    val excMsg = exc.getMessage
    assert(excMsg != null, "Message should not be null")
    println("\"" + excMsg + "\"")
  }

  @Test def testPlus(): Unit = {
    println("+")
    val expected = new Zeta8Int(0, 1, 1, 1)
    val actual = IMAG_UNIT_I + SQRT_NEG_2
    assertEquals(expected, actual)
  }

  @Test def testMinus(): Unit = {
    println("-")
    val expected = new Zeta8Int(0, -1, 1, -1)
    val actual = IMAG_UNIT_I - SQRT_NEG_2
    assertEquals(expected, actual)
  }

  @Test def testNegate(): Unit = {
    println("unary_-")
    assertEquals(NEG_ONE, -ONE)
    assertEquals(NEG_IMAG_UNIT_I, -IMAG_UNIT_I)
  }

  @Test def testTimes(): Unit = {
    println("*")
    var actual = IMAG_UNIT_I * SQRT_2
    assertEquals(SQRT_NEG_2, actual)
    var expected = new Zeta8Int(0, 0, 2, 0)
    actual = SQRT_NEG_2 * SQRT_2
    assertEquals(expected, actual)
    expected = new Zeta8Int(2, 0, 0, 0)
    actual = SQRT_2 * SQRT_2
    assertEquals(expected, actual)
    expected = new Zeta8Int(-2, 0, 0, 0)
    actual = SQRT_NEG_2 * SQRT_NEG_2
    assertEquals(expected, actual)
  }

  @Test def testTimesEightPowers(): Unit = {
    val eightZeta8s = List.fill(8)(ZETA_8)
    val actual = eightZeta8s.foldLeft(ONE)(_ * _)
    assertEquals(ONE, actual)
  }

  @Test def testDivides(): Unit = {
    println("/")
    try {
      var actual = SQRT_NEG_2 / SQRT_2
      assertEquals(IMAG_UNIT_I, actual)
      actual = SQRT_2 / SQRT_NEG_2
      assertEquals(NEG_IMAG_UNIT_I, actual)
      actual = SQRT_NEG_2 / IMAG_UNIT_I
      assertEquals(SQRT_2, actual)
      actual = ZETA_8_CUBED / ZETA_8
      assertEquals(IMAG_UNIT_I, actual)
      actual = ZETA_8_CUBED / IMAG_UNIT_I
      assertEquals(ZETA_8, actual)
    } catch {
      case nde: NotDivisibleException => fail(nde.getMessage)
      case e: Exception => println(e.getMessage)
        val msg = "Division should not have caused " + e.getClass.getName
        fail(msg)
    }
  }

  @Test def testTimesDivides(): Unit = {
    for {
      a <- -10 to -1
      b <- -5 to 5
      c <- -5 to 5
      d <- -5 to 5
    } {
      val multiplicandA = new Zeta8Int(a, b, c, d)
      val multiplicandB = new Zeta8Int(d, c, b, a)
      val product = multiplicandA * multiplicandB
      try {
        val divisionA = product / multiplicandB
        assertEquals(multiplicandA, divisionA)
        val divisionB = product / multiplicandA
        assertEquals(multiplicandB, divisionB)
      } catch {
        case nde: NotDivisibleException => fail(nde.getMessage)
        case e: Exception =>
          val msg = e.getClass.getName +
            " should not have occurred trying to divide " + product.toString +
            " by one of its divisors"
          fail(msg)
      }
    }
  }

  @Test def testDivisionByZero(): Unit = {
    try {
      val actual = ZETA_8 / ZERO
      val msg = "Trying to divide " + ZETA_8.toString +
        " by 0 should have caused an exception, not given result " +
        actual.toString
      fail(msg)
    } catch {
      case iae: IllegalArgumentException =>
        println("Trying to divide " + ZETA_8.toString
          + " by 0 correctly triggered IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
      case ae: ArithmeticException =>
        println("ArithmeticException is acceptable for trying to divide "
          + ZETA_8.toString + " by 0")
        println("\"" + ae.getMessage + "\"")
      case _: NotDivisibleException =>
        val msg = "NotDivisibleException is inappropriate for trying to divide " +
          ZETA_8.toString + " by 0"
        fail(msg)
      case e: Exception =>
        val msg = e.getClass.getName +
          " is not the right exception to throw for trying to divide " +
          ZETA_8.toString + " by 0"
        fail(msg)
    }
  }

}
