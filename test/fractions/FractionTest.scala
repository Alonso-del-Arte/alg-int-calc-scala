package fractions
// TODO: Switch over to JUnit 5
import org.junit.Test
import org.junit.Assert._

class FractionTest {

  @Test(expected = classOf[IllegalArgumentException]) def testConstructor(): Unit = {
    new Fraction(7, 0)
  }

  @Test def testImplicitConversionFromInt(): Unit = {
    val expected = new Fraction(49)
    val sevenSixths = new Fraction(7, 6)
    val actual = 42 * sevenSixths
    assertEquals(expected, actual)
  }

  @Test def testFractionsToLowestTerms(): Unit = {
    val oneHalf = new Fraction(1, 2)
    val twoQuarters = new Fraction(2, 4)
    assertEquals(oneHalf, twoQuarters)
  }

  @Test def testDefaultDenomOne(): Unit = {
    val seven = new Fraction(7)
    val sevenOneths = new Fraction(7, 1)
    assertEquals(seven, sevenOneths)
  }

  @Test def testApplyThreeQuarters(): Unit = {
    val expected = new Fraction(3, 4)
    val actual = Fraction(3, 4)
    assertEquals(expected, actual)
  }

  @Test def testApplyFourThirds(): Unit = {
    val expected = new Fraction(4, 3)
    val actual = Fraction(4, 3)
    assertEquals(expected, actual)
  }

  @Test def testToString(): Unit = {
    println("toString")
    val negOneHalf = new Fraction(-1, 2)
    val expected = "-1/2"
    val actual = negOneHalf.toString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testToStringDenomOneOmitted(): Unit = {
    val seven = new Fraction(7, 1)
    val expected = "7"
    val actual = seven.toString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testEquals(): Unit = {
    println("equals")
    val oneHalf = new Fraction(1, 2)
    val dupOneHalf = new Fraction(1, 2)
    assertEquals(oneHalf, dupOneHalf)
    val threeQuarters = new Fraction(3, 4)
    assertNotEquals(oneHalf, threeQuarters)
  }

  @Test def testHashCode(): Unit = {
    println("hashCode")
    val oneHalf = new Fraction(1, 2)
    val twoQuarters = new Fraction(2, 4)
    val oneHalfHashCode = oneHalf.hashCode
    val twoQuartersHashCode = twoQuarters.hashCode
    assertEquals(oneHalfHashCode, twoQuartersHashCode)
    val threeQuarters = new Fraction(3, 4)
    val threeQuartersHashCode = threeQuarters.hashCode
    assertNotEquals(oneHalfHashCode, threeQuartersHashCode)
  }

  @Test def testHashCodeUniqueness(): Unit = {
    val fractions = List.tabulate(720)(n => new Fraction(n - 1, 720))
    val hashes = fractions.map(_.hashCode)
    val uniqueHashCodes = hashes.toSet
    assertEquals(fractions.size, uniqueHashCodes.size)
  }

  @Test def testPlus(): Unit = {
    println("+")
    val threeHalves = new Fraction(3, 2)
    val fourSevenths = new Fraction(4, 7)
    val expected = new Fraction(29, 14)
    var actual = threeHalves + fourSevenths
    assertEquals(expected, actual)
    actual = fourSevenths + threeHalves // Commutative test
    assertEquals(expected, actual)
  }

  @Test def testNegate(): Unit = {
    println("unary_-")
    val threeHalves = new Fraction(3, 2)
    val expected = new Fraction(-3, 2)
    val actual = -threeHalves
    assertEquals(expected, actual)
  }

  @Test def testMinus(): Unit = {
    println("-")
    val threeHalves = new Fraction(3, 2)
    val fourSevenths = new Fraction(4, 7)
    val expected = new Fraction(13, 14)
    val actual = threeHalves - fourSevenths
    assertEquals(expected, actual)
  }

  @Test def testTimes(): Unit = {
    println("*")
    val threeHalves = new Fraction(3, 2)
    val fourSevenths = new Fraction(4, 7)
    val expected = new Fraction(6, 7)
    var actual = threeHalves * fourSevenths
    assertEquals(expected, actual)
    actual = fourSevenths * threeHalves // Commutative test
    assertEquals(expected, actual)
  }

  @Test def testDivides(): Unit = {
    println("/")
    val threeHalves = new Fraction(3, 2)
    val fourSevenths = new Fraction(4, 7)
    val expected = new Fraction(21, 8)
    val actual = threeHalves / fourSevenths
    assertEquals(expected, actual)
  }

  @Test def testDivisionByZero(): Unit = {
    val threeHalves = new Fraction(3, 2)
    val zero = new Fraction(0)
    try {
      val result = threeHalves / zero
      val msg = "Trying to divide by zero should have caused an exception, not given result " +
        result.toString
      fail(msg)
    } catch {
      case iae: IllegalArgumentException =>
        println("Trying to divide by zero correctly triggered IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
      case ae: ArithmeticException =>
        println("Trying to divide by zero correctly triggered ArithmeticException: " +
          ae.getMessage)
      case e: Exception => fail(e.getMessage)
    }
  }

  @Test def testReciprocal(): Unit = {
    println("reciprocal")
    val threeHalves = new Fraction(3, 2)
    val expected = new Fraction(2, 3)
    val actual = threeHalves.reciprocal
    assertEquals(expected, actual)
    try {
      val zero = new Fraction(0)
      val zeroRecip = zero.reciprocal
      val msg = "Trying to take reciprocal of 0 should have caused an exception, not given result " +
        zeroRecip.toString
      fail(msg)
    } catch {
      case iae: IllegalArgumentException =>
        println("Trying to take reciprocal of 0 correctly triggered IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
      case ae: ArithmeticException =>
        println("Trying to take reciprocal of 0 correctly triggered ArithmeticException")
        println("\"" + ae.getMessage + "\"")
      case e: Exception => fail(e.getMessage)
    }
  }

  @Test def testTo(): Unit = {
    println("to")
    val fourSevenths = new Fraction(4, 7)
    val threeHalves = new Fraction(3, 2)
    val fractionRange = fourSevenths to threeHalves
    for (n <- 8 to 21) assertEquals(new Fraction(n, 14), fractionRange(n - 8))
  }

  @Test def testNumericApproximation(): Unit = {
    println("numericApproximation")
    val oneSeventh = new Fraction(1, 7)
    var expected = 0.14285714
    val testDelta = 0.00000001
    var actual = oneSeventh.numericApproximation
    assertEquals(expected, actual, testDelta)
    val thirteenFiftyeights = new Fraction(13, 58)
    expected = 0.22413793
    actual = thirteenFiftyeights.numericApproximation
    assertEquals(expected, actual, testDelta)
  }

  @Test def testCompare(): Unit = {
    println("compare")
    val negThreeHalves = new Fraction(-3, 2)
    val approxPiFiftieths = new Fraction(157, 50)
    val approxPi113ths = new Fraction(355, 113)
    val approxPiSevenths = new Fraction(22, 7)
    val sevenHalves = new Fraction(7, 2)
    val mixedUp = List(approxPi113ths, approxPiFiftieths, approxPiSevenths,
      negThreeHalves, sevenHalves)
    val expected = List(negThreeHalves, approxPiFiftieths, approxPi113ths,
      approxPiSevenths, sevenHalves)
    val actual = mixedUp.sorted
    assertEquals(expected, actual)
  }

  @Test def testCompareThroughOperators(): Unit = {
    val negThreeHalves = new Fraction(-3, 2)
    val approxPiFiftieths = new Fraction(157, 50)
    val approxPi113ths = new Fraction(355, 113)
    val approxPiSevenths = new Fraction(22, 7)
    val sevenHalves = new Fraction(7, 2)
    assertTrue(negThreeHalves < approxPiFiftieths)
    assertTrue(approxPiFiftieths < approxPi113ths)
    assertTrue(approxPi113ths < approxPiSevenths)
    assertTrue(approxPiSevenths < sevenHalves)
    var equalFraction = new Fraction(-3, 2)
    assertTrue(negThreeHalves == equalFraction)
    equalFraction = new Fraction(157, 50)
    assertTrue(approxPiFiftieths == equalFraction)
    equalFraction = new Fraction(355, 113)
    assertTrue(approxPi113ths == equalFraction)
    equalFraction = new Fraction(22, 7)
    assertTrue(approxPiSevenths == equalFraction)
    equalFraction = new Fraction(7, 2)
    assertTrue(sevenHalves == equalFraction)
    assertTrue(approxPiFiftieths > negThreeHalves)
    assertTrue(approxPi113ths > approxPiFiftieths)
    assertTrue(approxPiSevenths > approxPi113ths)
    assertTrue(sevenHalves > approxPiSevenths)
  }

  @Test def testCompareThroughCollectionSort(): Unit = {
    val negThreeHalves = new Fraction(-3, 2)
    val approxPiFiftieths = new Fraction(157, 50)
    val approxPi113ths = new Fraction(355, 113)
    val approxPiSevenths = new Fraction(22, 7)
    val sevenHalves = new Fraction(7, 2)
    val unsortedList = List(approxPiFiftieths, sevenHalves, approxPiSevenths, negThreeHalves, approxPi113ths)
    val expected = List(negThreeHalves, approxPiFiftieths, approxPi113ths, approxPiSevenths, sevenHalves)
    val actual = unsortedList.sorted
    assertEquals(expected, actual)
  }

  @Test def testInferStep(): Unit = {
    println("inferStep")
    val begin = new Fraction(7, 12)
    val finish = new Fraction(13, 12)
    var expected = new Fraction(1, 12)
    var actual = Fraction.inferStep(begin, finish)
    assertEquals(expected, actual)
    expected = new Fraction(-1, 12)
    actual = Fraction.inferStep(finish, begin)
    assertEquals(expected, actual)
    val altBegin = new Fraction(5, 8)
    expected = new Fraction(1, 24)
    actual = Fraction.inferStep(altBegin, finish)
    assertEquals(expected, actual)
    expected = new Fraction(-1, 24)
    actual = Fraction.inferStep(finish, altBegin)
    assertEquals(expected, actual)
  }

}
