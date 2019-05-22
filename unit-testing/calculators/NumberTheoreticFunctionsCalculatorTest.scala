package calculators

import algebraics.NonEuclideanDomainException
import algebraics.quadratics.{ImagQuadRing, RealQuadRing}
import org.junit.Test
import org.junit.Assert._

class NumberTheoreticFunctionsCalculatorTest {

  val sieveLimit = 10000
  val primeSieve = new EratosthenesSieve(sieveLimit)
  val maxPrimePi = 1229

  @Test def testPrimeFactors() {
    println("primeFactors")
    val expected = Vector[Int](-1, 2, 7)
    val actual = NumberTheoreticFunctionsCalculator.primeFactors(-14)
    assertEquals(expected, actual)
    // TODO: Write test for AlgInt
  }

  @Test def testIsPrime() {
    println("isPrime")
    var number = 7
    var assertionMessage = number.toString + " should be found to be prime"
    assertTrue(assertionMessage, NumberTheoreticFunctionsCalculator.isPrime(number))
    number = 8
    assertionMessage = number.toString + " should not be found to be prime"
    assertFalse(assertionMessage, NumberTheoreticFunctionsCalculator.isPrime(number))
    var longNumber = 13L
    assertionMessage = longNumber.toString + " should be found to be prime"
    assertTrue(NumberTheoreticFunctionsCalculator.isPrime(longNumber))
    longNumber = 14L
    assertionMessage = longNumber.toString + " should not be found to be prime"
    assertFalse(NumberTheoreticFunctionsCalculator.isPrime(longNumber))
    for (i <- 0 until maxPrimePi) {
      val currPrime = primeSieve(i)
      assertionMessage = currPrime.toString + " should be found to be prime"
      assertTrue(assertionMessage, NumberTheoreticFunctionsCalculator.isPrime(currPrime))
      val currComposite = currPrime * 210
      assertionMessage = currComposite.toString + " should not be found to be prime"
      assertFalse(assertionMessage, NumberTheoreticFunctionsCalculator.isPrime(currComposite))
    }
    // TODO: Write test for AlgInt
  }

  @Test def testIsIrreducible() {
    println("isIrreducible")
    fail("Have not written test yet")
    // AlgInt
  }

  @Test def testIsDivisibleBy() {
    println("isDivisibleBy")
    fail("Have not written test yet")
    // AlgInt
  }

  @Test def testIsSquarefree() {
    println("isSquarefree")
    var assertionMessage = "The number -1 should be found to be squarefree"
    assertTrue(assertionMessage, NumberTheoreticFunctionsCalculator.isSquarefree(-1))
    assertionMessage = "The number 0 should not be found to be squarefree"
    assertFalse(assertionMessage, NumberTheoreticFunctionsCalculator.isSquarefree(0))
    assertionMessage = "The number 1 should be found to be squarefree"
    assertTrue(assertionMessage, NumberTheoreticFunctionsCalculator.isSquarefree(1))
    for (pIndex <- 0 to (maxPrimePi - 2)) {
      val p = primeSieve(pIndex)
      assertionMessage = "The number " + p.toString + " should be found to be squarefree"
      assertTrue(assertionMessage, NumberTheoreticFunctionsCalculator.isSquarefree(p))
      val q = -primeSieve(pIndex + 1)
      val pq = p * q
      assertionMessage = "The number " + pq.toString + " should be found to be squarefree"
      assertTrue(assertionMessage, NumberTheoreticFunctionsCalculator.isSquarefree(pq))
      val pSquared = p * p
      assertionMessage = "The number " + pSquared.toString + " should not be found to be squarefree"
      assertFalse(assertionMessage, NumberTheoreticFunctionsCalculator.isSquarefree(pSquared))
      if (p < 1290) {
      val pCubed = p * pSquared
      assertionMessage = "The number " + pCubed.toString + " should not be found to be squarefree"
      assertFalse(assertionMessage, NumberTheoreticFunctionsCalculator.isSquarefree(pCubed))
      if (p < 220) {
        val pCubedTimesQ = pCubed * q
        assertionMessage = "The number " + pCubedTimesQ.toString + " should not be found to be squarefree"
        assertFalse(assertionMessage, NumberTheoreticFunctionsCalculator.isSquarefree(pCubedTimesQ))
      }}
    }
  }

  @Test def testRandomSquarefreeNumber() {
    println("randomSquarefreeNumber")
    val specifiedBound = 65536
    val randomSquarefree = NumberTheoreticFunctionsCalculator.randomSquarefreeNumber(specifiedBound)
    println("Function came up with this potentially squarefree random number: " + randomSquarefree.toString)
    var assertionMessage = "Number said to be squarefree should not be divisible by 4"
    assertFalse(assertionMessage, randomSquarefree % 4 == 0)
    val squareRootFloor = Math.floor(Math.sqrt(randomSquarefree)).asInstanceOf[Int]
    for (i <- 3 to squareRootFloor by 2) {
      val square = i * i
      assertionMessage = "Number said to be squarefree should not be divisible by " + square.toString
      assertFalse(assertionMessage, randomSquarefree % square == 0)
    }
    assertionMessage = "Random number should not exceed specified bound of " + specifiedBound.toString
    assertTrue(assertionMessage, randomSquarefree <= specifiedBound)
  }

  @Test def testMoebiusMu() {
    println("moebiusMu")
    assertEquals(-1, NumberTheoreticFunctionsCalculator.moebiusMu(19))
    assertEquals(0, NumberTheoreticFunctionsCalculator.moebiusMu(20))
    assertEquals(1, NumberTheoreticFunctionsCalculator.moebiusMu(21))
    for (pIndex <- 0 to (maxPrimePi - 2)) {
      val currPrime = primeSieve(pIndex)
      assertEquals(-1, NumberTheoreticFunctionsCalculator.moebiusMu(currPrime))
      val squaredPrime = currPrime * currPrime
      assertEquals(0, NumberTheoreticFunctionsCalculator.moebiusMu(squaredPrime))
      val nextPrime = primeSieve(pIndex + 1)
      val semiPrime = currPrime * nextPrime
      assertEquals(1, NumberTheoreticFunctionsCalculator.moebiusMu(semiPrime))
    }
  }

  @Test def testSymbolLegendre() {
    println("symbolLegendre")
    assertEquals(-1, NumberTheoreticFunctionsCalculator.symbolLegendre(6, 7))
    assertEquals(0, NumberTheoreticFunctionsCalculator.symbolLegendre(7, 7))
    assertEquals(1, NumberTheoreticFunctionsCalculator.symbolLegendre(8, 7))
    for {pIndex <- 1 until maxPrimePi
         qIndex <- (pIndex + 1) to maxPrimePi} {
      val p = primeSieve(pIndex)
      val q = primeSieve(qIndex)
      assertEquals(0, NumberTheoreticFunctionsCalculator.symbolLegendre(p, p))
      (p % 4, q % 4) match {
        case (3, 3) => assertNotEquals(NumberTheoreticFunctionsCalculator.symbolLegendre(p, q), NumberTheoreticFunctionsCalculator.symbolLegendre(q, p))
        case _ => assertEquals(NumberTheoreticFunctionsCalculator.symbolLegendre(p, q), NumberTheoreticFunctionsCalculator.symbolLegendre(q, p))
      }
    }
  }

  @Test def testSymbolJacobi() {
    println("symbolJacobi")
    assertEquals(-1, NumberTheoreticFunctionsCalculator.symbolJacobi(104, 35))
    assertEquals(0, NumberTheoreticFunctionsCalculator.symbolJacobi(105, 35))
    assertEquals(1, NumberTheoreticFunctionsCalculator.symbolJacobi(106, 35))
  }

  @Test def testSymbolKronecker() {
    println("symbolKronecker")
    assertEquals(-1, NumberTheoreticFunctionsCalculator.symbolKronecker(31, 70))
    assertEquals(0, NumberTheoreticFunctionsCalculator.symbolKronecker(32, 70))
    assertEquals(1, NumberTheoreticFunctionsCalculator.symbolKronecker(33, 70))
  }

  //at Test def testSortListAlgebraicIntegersByNorm(listAlgInt: Vector[AlgInt]): Vector[AlgInt] = {
  //
  //}

  @Test def testKernel() {
    println("kernel")
    val expected = 14
    var num = 14
    var actual = NumberTheoreticFunctionsCalculator.kernel(num)
    assertEquals(expected, actual)
    num *= 2
    actual = NumberTheoreticFunctionsCalculator.kernel(num)
    assertEquals(expected, actual)
    num *= 7
    actual = NumberTheoreticFunctionsCalculator.kernel(num)
    assertEquals(expected, actual)
  }

  private def negCube(n: Int): Int = n * n * -n

  private def invalidFunctionF(n: Int): Int = 10

  @Test def testEuclideanGCD() {
    println("euclideanGCD")
    assertEquals(1, NumberTheoreticFunctionsCalculator.euclideanGCD(-27, 14))
    assertEquals(3, NumberTheoreticFunctionsCalculator.euclideanGCD(-27, 15))
    assertEquals(9, NumberTheoreticFunctionsCalculator.euclideanGCD(-27, 18))
    assertEquals(1, NumberTheoreticFunctionsCalculator.euclideanGCD(-21L, 13L))
    assertEquals(3, NumberTheoreticFunctionsCalculator.euclideanGCD(-21L, -144L))
    try {
      val result = NumberTheoreticFunctionsCalculator.euclideanGCD(-27, 18, negCube)
      println("gcd(-27, 18) with negative cube function gave this result: " + result)
      val failMsg = "gcd(-27, 18) with negative cube function should have triggered exception, not given result " + result
      fail(failMsg)
    } catch {
      case iae: IllegalArgumentException => println("gcd(-27, 18) with negative cube function correctly triggered IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is wrong exception to throw for gcd(-27, 18) with negative cube function"
        fail(failMsg)
    }
    try {
      val result = NumberTheoreticFunctionsCalculator.euclideanGCD(-27, 18, invalidFunctionF)
      println("gcd(-27, 18) with invalid function F gave this result: " + result)
      val failMsg = "gcd(-27, 18) with invalid function F should have triggered exception, not given result " + result
      fail(failMsg)
    } catch {
      case nede: NonEuclideanDomainException => println("gcd(-27, 18) with invalid function F correctly triggered NonEuclideanDomainException")
        println("\"" + nede.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is wrong exception to throw for gcd(-27, 18) with invalid function F"
        fail(failMsg)
    }
    // TODO: Write test for AlgInt
  }

  // at Test def testFundamentalUnit(ring: IntRing): AlgInt = {}

  // at Test def testPlaceInPrimarySector(num: AlgInt): AlgInt = {}

  // at Test def testDivideOutUnits(num: AlgInt): AlgInt = {}

  // at Test def testGetOneInRing(ring: IntRing): AlgInt = {}

  @Test def testFieldClassNumber() {
    println("fieldClassNumber")
    val numbersHeegner = Array(-1, -2, -3, -7, -11, -19, -43, -67, -163)
    for (d <- numbersHeegner) {
      val imagUFD = new ImagQuadRing(d)
      assertEquals(1, NumberTheoreticFunctionsCalculator.fieldClassNumber(imagUFD))
      val imagNonUFD = new ImagQuadRing(5 * d)
      assertTrue(NumberTheoreticFunctionsCalculator.fieldClassNumber(imagNonUFD) > 1)
    }
    val selRealUFDDiscrs = Array(2, 3, 6, 7, 11, 13, 17, 19, 21, 22, 23, 29)
    for (dR <- selRealUFDDiscrs) {
      val realUFD = new RealQuadRing(dR)
      assertEquals(1, NumberTheoreticFunctionsCalculator.fieldClassNumber(realUFD))
      val realNonUFD = new RealQuadRing(5 * dR)
      assertTrue(NumberTheoreticFunctionsCalculator.fieldClassNumber(realNonUFD) > 1)
    }
  }

}
