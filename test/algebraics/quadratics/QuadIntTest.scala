package algebraics.quadratics

import algebraics.{AlgebraicDegreeOverflowException, NotDivisibleException, UnsupportedNumberDomainException}

import org.junit.Test
import org.junit.Assert._

object QuadIntTest {

  val TEST_DELTA = 0.00000001

}

class QuadIntTest {
  private val ringGaussian = new ImagQuadRing(-1)
  private val ringEisenstein = new ImagQuadRing(-3)
  private val ringZ2 = new RealQuadRing(2)
  private val ringZPhi = new RealQuadRing(5)

  @Test def testApply(): Unit = {
    println("apply")
    fail("REWRITE THIS TEST")
//    var expected: QuadInt = new ImagQuadInt(12, 7, ringGaussian, 1)
//    var actual: QuadInt = QuadInt(12, 7, ringGaussian)
//    assertEquals(expected, actual)
//    expected = new ImagQuadInt(25, 7, ringEisenstein, 2)
//    actual = QuadInt(25, 7, ringEisenstein, 2)
//    assertEquals(expected, actual)
//    expected = new RealQuadInt(15, 4, ringZ2, 1)
//    actual = QuadInt(15, 4, ringZ2)
//    assertEquals(expected, actual)
//    try {
//      actual = QuadInt(10, 10, ringUnsupported)
//      val failMsg = "Trying to create " + actual.toString + " should have caused an exception"
//      fail(failMsg)
//    } catch {
//      case unde: UnsupportedNumberDomainException => println("Trying to use unsupported number domain correctly triggered UnsupportedNumberDomainException")
//        println("\"" + unde.getMessage + "\"")
//      case e: Exception => val failMsg = e.getClass.getName + " is not the right exception for trying to use an unsupported number domain"
//        fail(failMsg)
//    }
  }

  @Test def testPlusUnaryAsQuadratic(): Unit = {
    println("Testing that plus can perform operation when appropriate")
    val testAddendA = new ImagQuadInt(-1, 1, ringEisenstein, 2)
    val testAddendB = new RealQuadInt(7, 0, ringZ2)
    val expected = new ImagQuadInt(13, 1, ringEisenstein, 2)
    var actual = testAddendA + testAddendB
    assertEquals(expected, actual)
    // Commutative check
    actual = testAddendB + testAddendA
    assertEquals(expected, actual)
  }

  @Test def testPlusUnary(): Unit = {
    val testAddend = new ImagQuadInt(-1, 1, ringEisenstein, 2)
    val expected = new ImagQuadInt(13, 1, ringEisenstein, 2)
    val actual = testAddend + 7
    assertEquals(expected, actual)
  }

  @Test def testPlusDegreeOverflow(): Unit = {
    println("Testing that plus throws exception when result is of degree 4")
    val testAddendA = new ImagQuadInt(-1, 1, ringEisenstein, 2)
    val testAddendB = new RealQuadInt(7, 1, ringZ2)
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

  @Test def testMinusUnary(): Unit = {
    val testMinuend = new RealQuadInt(-1, 1, ringZ2)
    val expected = new RealQuadInt(-8, 1, ringZ2)
    val actual = testMinuend - 7
    assertEquals(expected, actual)
  }

  @Test def testTimesUnaryAsQuadratic(): Unit = {
    println("Testing that multiplying by a degree 1 algebraic integer presented as quadratic integer gives result in appropriate quadratic ring")
    val testMultiplicandA = new ImagQuadInt(5, -1, ringEisenstein, 2)
    val testMultiplicandB = new RealQuadInt(2, 0, ringZPhi)
    val expected = new ImagQuadInt(5, -1, ringEisenstein)
    var assertionMessage = testMultiplicandA.toString + " times " + testMultiplicandB.toString + " should be " + expected.toString
    var actual = testMultiplicandA * testMultiplicandB
    assertEquals(assertionMessage, expected, actual)
    // Commutative check
    assertionMessage = testMultiplicandB.toString + " times " + testMultiplicandA.toString + " should be " + expected.toString
    actual = testMultiplicandB * testMultiplicandA
    assertEquals(expected, actual)
  }

  @Test def testTimesUnary(): Unit = {
    val testMultiplicand = new ImagQuadInt(5, -1, ringEisenstein, 2)
    val expected = new ImagQuadInt(5, -1, ringEisenstein)
    val actual = testMultiplicand * 2
    assertEquals(expected, actual)
  }

  @Test def testTimesCrossDomain(): Unit = {
    println("Testing that times can correctly go from real to imaginary domain and back when appropriate")
    val testMultiplicandA = new ImagQuadInt(0, 2, ringEisenstein)
    val testMultiplicandB = new RealQuadInt(0, 7, ringZPhi)
    val expectedRing = new ImagQuadRing(-15)
    val expected = new ImagQuadInt(0, 14, expectedRing)
    var assertionMessage = testMultiplicandA.toString + " times " + testMultiplicandB.toString + " should be " + expected.toString
    var actual = testMultiplicandA * testMultiplicandB
    assertEquals(assertionMessage, expected, actual)
    // Commutative check
    assertionMessage = testMultiplicandB.toString + " times " + testMultiplicandA.toString + " should be " + expected.toString
    actual = testMultiplicandB * testMultiplicandA
    assertEquals(assertionMessage, expected, actual)
  }

  @Test def testTimesCrossDomainResult(): Unit = {
    println("Testing that times can correctly go to real number from multiplying two imaginary numbers of different quadratic domains")
    val testMultiplicandA = new ImagQuadInt(0, 2, ringGaussian)
    val ringZi5 = new ImagQuadRing(-5)
    val testMultiplicandB = new ImagQuadInt(0, 7, ringZi5)
    val expected = new RealQuadInt(0, -14, ringZPhi)
    var assertionMessage = testMultiplicandA.toString + " times " + testMultiplicandB.toString + " should be " + expected.toString
    var actual = testMultiplicandA * testMultiplicandB
    assertEquals(assertionMessage, expected, actual)
    // Commutative check
    assertionMessage = testMultiplicandB.toString + " times " + testMultiplicandA.toString + " should be " + expected.toString
    actual = testMultiplicandB * testMultiplicandA
    assertEquals(expected, actual)
  }

  @Test def testTimesDegreeOverflow(): Unit = {
    println("Testing that times throws exception when result is of degree 4")
    val testMultiplicandA = new ImagQuadInt(-1, 1, ringEisenstein, 2)
    val testMultiplicandB = new RealQuadInt(7, 1, ringZ2)
    try {
      val result = testMultiplicandA * testMultiplicandB
      val failMsg = "Trying to multiply " + testMultiplicandA.toString + " by " + testMultiplicandB.toString + " should have caused an exception, not given result " + result.toString
      fail(failMsg)
    } catch {
      case adoe: AlgebraicDegreeOverflowException => println("Trying to multiply " + testMultiplicandA.toString + " by " + testMultiplicandB.toString + " correctly caused AlgebraicDegreeOverflowException")
        println("\"" + adoe.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is the wrong exception for trying to multiply " + testMultiplicandA.toString + " by " + testMultiplicandB.toString
        fail(failMsg)
    }
  }

  @Test def testDivideQuadraticByUnary(): Unit = {
    println("Dividing quadratic by unary gives appropriate result even if unary presented as quadratic from other ring")
    val testDividend = new ImagQuadInt(5, -1, ringEisenstein)
    val testDivisor = new RealQuadInt(2, 0, ringZPhi)
    val expected = new ImagQuadInt(5, -1, ringEisenstein, 2)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case _: NotDivisibleException => val failMsg = "Since " + testDividend.toString + " is divisible by " + testDivisor.toString + ", dividing should not have caused NotDivisibleException"
        fail(failMsg)
      case e: Exception => val failMsg = e.getClass.getName + " should not have occurred for dividing " + testDividend.toString + " by " + testDivisor.toString
        fail(failMsg)
    }
  }

  @Test def testDivideUnaryByQuadratic(): Unit = {
    println("Dividing unary by quadratic gives appropriate result even if unary presented as quadratic from other ring")
    val testDividend = new RealQuadInt(2, 0, ringZPhi)
    val testDivisor = new ImagQuadInt(1, 1, ringGaussian)
    val expected = new ImagQuadInt(1, -1, ringGaussian)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case _: NotDivisibleException => val failMsg = "Since " + testDividend.toString + " is divisible by " + testDivisor.toString + ", dividing should not have caused NotDivisibleException"
        fail(failMsg)
      case e: Exception => val failMsg = e.getClass.getName + " should not have occurred for dividing " + testDividend.toString + " by " + testDivisor.toString
        fail(failMsg)
    }
  }

  @Test def testDivideByUnary(): Unit = {
    val testDividend = new RealQuadInt(3, 7, ringZPhi)
    val expected = new RealQuadInt(3, 7, ringZPhi, 2)
    val actual = testDividend / 2
    assertEquals(expected, actual)
  }

  @Test def testDivideCrossDomain(): Unit = {
    println("Testing that divides can correctly go from real to imaginary domain and back when appropriate")
    val dividendRing = new ImagQuadRing(-15)
    val testDividend = new ImagQuadInt(0, 14, dividendRing)
    var testDivisor: QuadInt = new ImagQuadInt(0, 2, ringEisenstein)
    var expected: QuadInt = new RealQuadInt(0, 7, ringZPhi)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case _: NotDivisibleException => val failMsg = "Since " + testDividend.toString + " is divisible by " + testDivisor.toString + ", dividing should not have caused NotDivisibleException"
        fail(failMsg)
      case e: Exception => println("\"" + e.getMessage + "\"")
        val failMsg = e.getClass.getName + " should not have occurred for dividing " + testDividend.toString + " by " + testDivisor.toString
        fail(failMsg)
    }
    testDivisor = expected
    expected = new ImagQuadInt(0, 2, ringEisenstein)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case _: NotDivisibleException => val failMsg = "Since " + testDividend.toString + " is divisible by " + testDivisor.toString + ", dividing should not have caused NotDivisibleException"
        fail(failMsg)
      case e: Exception => val failMsg = e.getClass.getName + " should not have occurred for dividing " + testDividend.toString + " by " + testDivisor.toString
        fail(failMsg)
    }
  }

  @Test def testDividesCrossDomainResult(): Unit = {
    println("Testing that divides can correctly go from real number to imaginary number from dividing a real number by an imaginary number")
    val testDividend = new RealQuadInt(0, -14, ringZPhi)
    var testDivisor = new ImagQuadInt(0, 2, ringGaussian)
    val ringZi5 = new ImagQuadRing(-5)
    var expected = new ImagQuadInt(0, -7, ringZi5)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case _: NotDivisibleException => val failMsg = "Since " + testDividend.toString + " is divisible by " + testDivisor.toString + ", dividing should not have caused NotDivisibleException"
        fail(failMsg)
      case e: Exception => val failMsg = e.getClass.getName + " should not have occurred for dividing " + testDividend.toString + " by " + testDivisor.toString
        fail(failMsg)
    }
    testDivisor = expected
    expected = new ImagQuadInt(0, 2, ringGaussian)
    try {
      val actual = testDividend / testDivisor
      assertEquals(expected, actual)
    } catch {
      case _: NotDivisibleException => val failMsg = "Since " + testDividend.toString + " is divisible by " + testDivisor.toString + ", dividing should not have caused NotDivisibleException"
        fail(failMsg)
      case e: Exception => val failMsg = e.getClass.getName + " should not have occurred for dividing " + testDividend.toString + " by " + testDivisor.toString
        fail(failMsg)
    }
  }

  @Test def testDividesDegreeOverflow(): Unit = {
    println("Testing that divides throws exception when result is of degree 4")
    val testDividend = new RealQuadInt(0, 2, ringZ2)
    val testDivisor = new ImagQuadInt(1, 1, ringGaussian)
    try {
      val result = testDividend / testDivisor
      val failMsg = "Trying to divide " + testDividend.toString + " by " + testDivisor.toString + " should have caused an exception, not given result " + result.toString
      fail(failMsg)
    } catch {
      case adoe: AlgebraicDegreeOverflowException => println("Trying to divide " + testDividend.toString + " by " + testDivisor.toString + " correctly caused AlgebraicDegreeOverflowException")
        println("\"" + adoe.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is the wrong exception for trying to divide " + testDividend.toString + " by " + testDivisor.toString
        fail(failMsg)
    }
  }

  @Test def testDivideByZero(): Unit = {
    println("Testing that division by zero triggers the correct exception")
    val testDividend = new RealQuadInt(5, 8, ringZ2)
    try {
      val result = testDividend / 0
      val failMsg = "Trying to divide " + testDividend.toString + " by 0 should have caused an exception, not given result " + result.toString
      fail(failMsg)
    } catch {
      case nde: NotDivisibleException => val failMsg = "Since " + testDividend.toString + " divided by 0 is not an algebraic number, NotDivisibleException is inappropriate"
        println(failMsg)
        println("\"" + nde.getMessage + "\"")
        fail(failMsg)
      case iae: IllegalArgumentException => println("Trying to divide " + testDividend.toString + " by 0 correctly triggered IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
      case ae: ArithmeticException => println("ArithmeticException is adequate for trying to divide " + testDividend.toString + " by 0")
        println("\"" + ae.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is not the right exception for trying to divide " + testDividend.toString + " by 0"
        fail(failMsg)
    }
  }

}
