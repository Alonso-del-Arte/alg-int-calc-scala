package fractions

import org.junit.Test
import org.junit.Assert._

class FractionRangeTest {

  @Test def testConstructor(): Unit = {
    println("constructor")
    val begin = new Fraction(5, 8)
    val finish = new Fraction(7, 8)
    val invalidStep = new Fraction(3, 2)
    try {
      val fractionRange = new FractionRange(begin, finish, invalidStep)
      val failMsg = "Should not have been able to construct FractionRange " + fractionRange.toString + " with invalid step " + invalidStep.toString
      fail(failMsg)
    } catch {
      case iae: IllegalArgumentException => println("Trying to construct FractionRange with invalid step " + invalidStep.toString + " correctly triggered IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is wrong exception for trying to construct FractionRange with invalid step " + invalidStep.toString + "."
        fail(failMsg)
    }
  }

  @Test def testToString(): Unit = {
    println("toString")
    val begin = new Fraction(7, 12)
    val finish = new Fraction(13, 12)
    var fractionRange = new FractionRange(begin, finish)
    var expected = "7/12to13/12"
    var actual = fractionRange.toString.replace(" ", "")
    assertEquals(expected, actual)
    val newStepSize = new Fraction(1, 24)
    fractionRange = new FractionRange(begin, finish, newStepSize)
    expected = "7/12to13/12by1/24"
    actual = fractionRange.toString.replace(" ", "")
    assertEquals(expected, actual)
  }

  @Test def testEquals(): Unit = {
    println("equals")
    val begin = new Fraction(7, 12)
    val finish = new Fraction(13, 12)
    val fractionRange = new FractionRange(begin, finish)
    val explicitStep = new Fraction(1, 12)
    val sameFractionRange = new FractionRange(begin, finish, explicitStep)
    assertEquals(fractionRange, sameFractionRange)
    val diffFractionRange = new FractionRange(explicitStep, finish)
    assertNotEquals(fractionRange, diffFractionRange)
  }

  @Test def testHashCode(): Unit = {
    println("hashCode")
    val begin = new Fraction(7, 12)
    val finish = new Fraction(13, 12)
    val fractionRange = new FractionRange(begin, finish)
    val explicitStep = new Fraction(1, 12)
    val sameFractionRange = new FractionRange(begin, finish, explicitStep)
    assertEquals(fractionRange.hashCode, sameFractionRange.hashCode)
    val diffFractionRange = new FractionRange(explicitStep, finish)
    assertNotEquals(fractionRange.hashCode, diffFractionRange.hashCode)
    println(fractionRange.toString + " hashed as " + fractionRange.hashCode)
    println(diffFractionRange.toString + " hashed as " + diffFractionRange.hashCode)
  }

  @Test def testLength(): Unit = {
    println("length")
    val begin = new Fraction(7, 12)
    val finish = new Fraction(13, 12)
    var fractionRange = new FractionRange(begin, finish)
    var expected = 7
    var actual = fractionRange.length
    assertEquals(expected, actual)
    val newStepSize = new Fraction(1, 24)
    fractionRange = new FractionRange(begin, finish, newStepSize)
    expected = 13
    actual = fractionRange.length
    assertEquals(expected, actual)
  }

  @Test def testApply(): Unit = {
    println("apply")
    val begin = new Fraction(7, 12)
    val finish = new Fraction(13, 12)
    var fractionRange = new FractionRange(begin, finish)
    for (n <- 7 to 13) {
      val expected = new Fraction(n, 12)
      val actual = fractionRange.apply(n - 7)
      assertEquals(expected, actual)
    }
    try {
      val result: Fraction = fractionRange.apply(7)
      val failMsg = "Trying to apply index 7 to range " + fractionRange.toString + " should have caused an exception, not given result " + result.toString
      fail(failMsg)
    } catch {
      case ioobe: IndexOutOfBoundsException => println("Trying to apply index 7 to range " + fractionRange.toString + " correctly caused IndexOutOfBoundsException")
        println("\"" + ioobe.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is wrong exception for trying to apply index 7 to range " + fractionRange.toString + "."
        fail(failMsg)
    }
    fractionRange = new FractionRange(finish, begin)
    for (m <- 13 to 7 by -1) {
      val expected = new Fraction(m, 12)
      val actual = fractionRange.apply(-m + 13)
      assertEquals(expected, actual)
    }
    val newStepSize = new Fraction(1, 24)
    fractionRange = new FractionRange(begin, finish, newStepSize)
    for (m <- 14 to 26) {
      val expected = new Fraction(m, 24)
      val actual = fractionRange.apply(m - 14)
      assertEquals(expected, actual)
    }
    try {
      val result: Fraction = fractionRange.apply(-1)
      val failMsg = "Trying to apply index -1 to range " + fractionRange.toString + " should have caused an exception, not given result " + result.toString
      fail(failMsg)
    } catch {
      case ioobe: IndexOutOfBoundsException => println("Trying to apply index -1 to range " + fractionRange.toString + " correctly caused IndexOutOfBoundsException")
        println("\"" + ioobe.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is wrong exception for trying to apply index -1 to range " + fractionRange.toString + "."
      fail(failMsg)
    }
    try {
      val result: Fraction = fractionRange.apply(200)
      val failMsg = "Trying to apply index 200 to range " + fractionRange.toString + " should have caused an exception, not given result " + result.toString
      fail(failMsg)
    } catch {
      case ioobe: IndexOutOfBoundsException => println("Trying to apply index 200 to range " + fractionRange.toString + " correctly caused IndexOutOfBoundsException")
        println("\"" + ioobe.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is wrong exception for trying to apply index 200 to range " + fractionRange.toString + "."
        fail(failMsg)
    }
  }

  @Test def testBy(): Unit = {
    println("by")
    val begin = new Fraction(0)
    val finish = new Fraction(1)
    val interval = new Fraction(1, 144)
    val initRange = new FractionRange(begin, finish, interval)
    val newInterval = new Fraction(1, 16)
    val expected = new FractionRange(begin, finish, newInterval)
    val actual = initRange.by(newInterval)
    assertEquals(expected, actual)
    assertNotEquals(initRange, actual)
  }

}
