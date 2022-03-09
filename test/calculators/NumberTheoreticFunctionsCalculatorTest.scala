package calculators

import algebraics.{AlgInt, AlgebraicDegreeOverflowException, NonEuclideanDomainException, UnsupportedNumberDomainException}
import algebraics.quadratics.{IllDefQuadInt, IllDefQuadRing, ImagQuadInt, ImagQuadRing, QuadInt, RealQuadInt, RealQuadRing}

import org.junit.Test
import org.junit.Assert._

object NumberTheoreticFunctionsCalculatorTest {

  def adj23Norm(num: AlgInt): Long = {
    var interim = Math.abs(num.norm)
    while (NumberTheoreticFunctionsCalculator.euclideanGCD(interim, 23L) > 1 && interim > 0) {
      interim /= 23
      interim *= 26
    }
    interim
  }

  def invalidFunctionF(num: AlgInt): Long = -Math.abs(num.norm)

}

class NumberTheoreticFunctionsCalculatorTest {
  val sieveLimit = 10000
  val primeSieve = new EratosthenesSieve(sieveLimit)
  val maxPrimePi = 1229
  val primePi1K = 168
  private val ringGaussian = new ImagQuadRing(-1)
  private val ringEisenstein = new ImagQuadRing(-3)
  private val ringZ2 = new RealQuadRing(2)
  private val ringZPhi = new RealQuadRing(5)
  private val ringUnsupported = new IllDefQuadRing(1)

  @Test def testPrimeFactors(): Unit = {
    println("primeFactors")
    val expected = Vector[Int](-1, 2, 7)
    val actual = NumberTheoreticFunctionsCalculator.primeFactors(-14)
    assertEquals(expected, actual)
  }

  @Test def testPrimeFactorsAlgInt(): Unit = {
    val negOne = new RealQuadInt(-1, 0, ringZ2)
    val sqrt2 = new RealQuadInt(0, 1, ringZ2)
    val norm7 = new RealQuadInt(3, 1, ringZ2)
    val toBeFactored = new RealQuadInt(-12, -11, ringZ2)
    val expected = Vector[RealQuadInt](negOne, sqrt2, norm7, norm7)
    val actual = NumberTheoreticFunctionsCalculator.primeFactors(toBeFactored)
    assertEquals(expected, actual)
  }

  @Test def testIsPrime(): Unit = {
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

  @Test def testIsIrreducible(): Unit = {
    println("isIrreducible")
    val ringZi5 = new ImagQuadRing(-5)
    var num = new ImagQuadInt(1, 1, ringZi5)
    var msg = num.toString + " should be found to be irreducible"
    assertTrue(msg, NumberTheoreticFunctionsCalculator.isIrreducible(num))
    num = new ImagQuadInt(1, 7, ringZi5)
    msg = num.toString + " should not be found to be irreducible"
    assertFalse(msg, NumberTheoreticFunctionsCalculator.isIrreducible(num))
  }

  @Test def testIsDivisibleBy(): Unit = {
    println("isDivisibleBy")
    val testDividend = new RealQuadInt(59, 0, ringZPhi)
    val testDivisor = new RealQuadInt(8, 1, ringZPhi)
    var assertionMessage = testDividend.toString + " should be found to be divisible by " + testDivisor.toString
    assertTrue(assertionMessage, NumberTheoreticFunctionsCalculator.isDivisibleBy(testDividend, testDivisor))
    assertionMessage = testDivisor.toString + " should not be found to be divisible by " + testDividend.toString
    assertFalse(assertionMessage, NumberTheoreticFunctionsCalculator.isDivisibleBy(testDivisor, testDividend))
  }

  @Test def testIsSquarefree(): Unit = {
    println("isSquarefree")
    var msg = "The number -1 should be found to be squarefree"
    assertTrue(msg, NumberTheoreticFunctionsCalculator.isSquarefree(-1))
    msg = "The number 0 should not be found to be squarefree"
    assertFalse(msg, NumberTheoreticFunctionsCalculator.isSquarefree(0))
    msg = "The number 1 should be found to be squarefree"
    assertTrue(msg, NumberTheoreticFunctionsCalculator.isSquarefree(1))
    for (pIndex <- 0 to (maxPrimePi - 2)) {
      val p = primeSieve(pIndex)
      msg = "The number " + p.toString + " should be found to be squarefree"
      assertTrue(msg, NumberTheoreticFunctionsCalculator.isSquarefree(p))
      val q = -primeSieve(pIndex + 1)
      val pq = p * q
      msg = "The number " + pq.toString + " should be found to be squarefree"
      assertTrue(msg, NumberTheoreticFunctionsCalculator.isSquarefree(pq))
      val pSquared = p * p
      msg = "The number " + pSquared.toString + " should not be found to be squarefree"
      assertFalse(msg, NumberTheoreticFunctionsCalculator.isSquarefree(pSquared))
      if (p < 1290) {
      val pCubed = p * pSquared
      msg = "The number " + pCubed.toString + " should not be found to be squarefree"
      assertFalse(msg, NumberTheoreticFunctionsCalculator.isSquarefree(pCubed))
      if (p < 220) {
        val pCubedTimesQ = pCubed * q
        msg = "The number " + pCubedTimesQ.toString + " should not be found to be squarefree"
        assertFalse(msg, NumberTheoreticFunctionsCalculator.isSquarefree(pCubedTimesQ))
      }}
    }
  }

  @Test def testRandomSquarefreeNumber(): Unit = {
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

  @Test def testMoebiusMu(): Unit = {
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

  @Test def testLegendreSymbolP7Cases(): Unit = {
    assertEquals(-1, NumberTheoreticFunctionsCalculator.symbolLegendre(6, 7))
    assertEquals(0, NumberTheoreticFunctionsCalculator.symbolLegendre(7, 7))
    assertEquals(1, NumberTheoreticFunctionsCalculator.symbolLegendre(8, 7))
  }

  @Test def testLegendreSymbol(): Unit = {
    println("symbolLegendre")
    val quadRecipMsg = "Quadratic reciprocity applies: ("
    for {pIndex <- 1 until primePi1K
         qIndex <- (pIndex + 1) to primePi1K} {
      val p = primeSieve(pIndex)
      val q = primeSieve(qIndex)
      assertEquals(0, NumberTheoreticFunctionsCalculator.symbolLegendre(p, p))
      (p % 4, q % 4) match {
        case (3, 3) => val msg = "(" + p.toString + ", " + q.toString + ") = -(" + q.toString + ", " + p.toString + ")"
          assertNotEquals(msg, NumberTheoreticFunctionsCalculator.symbolLegendre(p, q), NumberTheoreticFunctionsCalculator.symbolLegendre(q, p))
        case _ => val assertionMessage = quadRecipMsg + p.toString + ", " + q.toString + ") = (" + q.toString + ", " + p.toString + ")"
          assertEquals(assertionMessage, NumberTheoreticFunctionsCalculator.symbolLegendre(p, q), NumberTheoreticFunctionsCalculator.symbolLegendre(q, p))
      }
    }
  }

  @Test def testLegendreSymbolBadArgEvenPrime(): Unit = {
    try {
      val result = NumberTheoreticFunctionsCalculator.symbolLegendre(7, 2)
      fail("Calling Legendre(7, 2) should have triggered an exception, not given result " + result.toString)
    } catch {
      case iae: IllegalArgumentException => println("Calling Legendre(7, 2) correctly triggered IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
    }
  }

  @Test def testLegendreSymbolBadArgs(): Unit = {
    try {
      val result = NumberTheoreticFunctionsCalculator.symbolLegendre(12, 35)
      fail("Calling Legendre(12, 35) should have triggered an exception, not given result " + result.toString)
    } catch {
      case iae: IllegalArgumentException => println("Calling Legendre(12, 35) correctly triggered IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
    }
  }

  @Test def testJacobiSymbolM35Cases(): Unit = {
    assertEquals(-1, NumberTheoreticFunctionsCalculator.symbolJacobi(104, 35))
    assertEquals(0, NumberTheoreticFunctionsCalculator.symbolJacobi(105, 35))
    assertEquals(1, NumberTheoreticFunctionsCalculator.symbolJacobi(106, 35))
  }

  @Test def testJacobiLegendreCorrespondence(): Unit = {
    for {pindex <- 1 until primePi1K
         qindex <- pindex until primePi1K} {
      val p = primeSieve(pindex)
      val q = primeSieve(qindex)
      val assertionMessage = "Jacobi(" + p.toString + ", " + q.toString + ") should be equal to Legendre(" + p.toString + ", " + q.toString + ")"
      val expected = NumberTheoreticFunctionsCalculator.symbolLegendre(p, q)
      val actual = NumberTheoreticFunctionsCalculator.symbolJacobi(p, q)
      assertEquals(assertionMessage, expected, actual)
    }
    println("Jacobi-Legendre correspondence checks out...")
  }

  @Test def testJacobiSymbol(): Unit = {
    println("symbolJacobi")
    for {pindex <- 1 until primePi1K
         qindex <- pindex until primePi1K} {
      val p = primeSieve(pindex)
      val q = primeSieve(qindex)
      val m = p * q
      for (n <- 15 to 20) {
        val expected = NumberTheoreticFunctionsCalculator.symbolLegendre(n, p) *
          NumberTheoreticFunctionsCalculator.symbolLegendre(n, q)
        val actual = NumberTheoreticFunctionsCalculator.symbolJacobi(n, m)
        assertEquals(expected, actual)
      }
    }
  }

  @Test def testJacobiSymbolBadArgEvenM(): Unit = {
    try {
      val result = NumberTheoreticFunctionsCalculator.symbolJacobi(7, 2)
      val failMsg = "Calling Jacobi(7, 2) should have caused an exception, not given result " + result.toString
      fail(failMsg)
    } catch {
      case iae: IllegalArgumentException => println("Calling Jacobi(7, 2) correctly caused IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is the wrong exception to throw for calling Jacobi(7, 2)"
        fail(failMsg)
    }
  }

  @Test def testJacobiSymbolBadArgNegM(): Unit = {
    try {
      val result = NumberTheoreticFunctionsCalculator.symbolJacobi(7, -3)
      val failMsg = "Calling Jacobi(7, -3) should have caused an exception, not given result " + result.toString
      fail(failMsg)
    } catch {
      case iae: IllegalArgumentException => println("Calling Jacobi(7, -3) correctly caused IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is the wrong exception to throw for calling Jacobi(7, -3)"
        fail(failMsg)
    }
  }

  @Test def testKroneckerSymbolM70CasesWNegN(): Unit = {
    assertEquals(-1, NumberTheoreticFunctionsCalculator.symbolKronecker(-33, 70))
    assertEquals(0, NumberTheoreticFunctionsCalculator.symbolKronecker(-32, 70))
    assertEquals(1, NumberTheoreticFunctionsCalculator.symbolKronecker(-31, 70))
  }

  @Test def testKroneckerSymbolM70Cases(): Unit = {
    assertEquals(-1, NumberTheoreticFunctionsCalculator.symbolKronecker(31, 70))
    assertEquals(0, NumberTheoreticFunctionsCalculator.symbolKronecker(32, 70))
    assertEquals(1, NumberTheoreticFunctionsCalculator.symbolKronecker(33, 70))
  }

  @Test def testKroneckerLegendreCorrespondence(): Unit = {
    for {pindex <- 1 until primePi1K
         qindex <- pindex until primePi1K} {
      val p = primeSieve(pindex)
      val q = primeSieve(qindex)
      val assertionMessage = "Kronecker(" + p.toString + ", " + q.toString + ") should be equal to Legendre(" + p.toString + ", " + q.toString + ")"
      val expected = NumberTheoreticFunctionsCalculator.symbolLegendre(p, q)
      val actual = NumberTheoreticFunctionsCalculator.symbolKronecker(p, q)
      assertEquals(assertionMessage, expected, actual)
    }
    println("Kronecker-Legendre correspondence checks out...")
  }

  @Test def testKroneckerJacobiCorrespondence(): Unit = {
    for {j <- -10 to 10
         b <- 5 to 15 by 2} {
      val expected = NumberTheoreticFunctionsCalculator.symbolJacobi(j, b)
      val actual = NumberTheoreticFunctionsCalculator.symbolKronecker(j, b)
      assertEquals(expected, actual)
    }
    println("Kronecker-Jacobi correspondence checks out...")
  }

  @Test def testKroneckerSymbol(): Unit = {
    println("symbolKronecker")
    for (m <- -24 to 24 by 8) {
      assertEquals(-1, NumberTheoreticFunctionsCalculator.symbolKronecker(m + 3, 2))
      assertEquals(-1, NumberTheoreticFunctionsCalculator.symbolKronecker(m + 5, 2))
      assertEquals(0, NumberTheoreticFunctionsCalculator.symbolKronecker(m, 2))
      assertEquals(0, NumberTheoreticFunctionsCalculator.symbolKronecker(m + 2, 2))
      assertEquals(0, NumberTheoreticFunctionsCalculator.symbolKronecker(m + 4, 2))
      assertEquals(0, NumberTheoreticFunctionsCalculator.symbolKronecker(m + 6, 2))
      assertEquals(1, NumberTheoreticFunctionsCalculator.symbolKronecker(m + 1, 2))
      assertEquals(1, NumberTheoreticFunctionsCalculator.symbolKronecker(m + 7, 2))
      val expected = Integer.signum(m + 1)
      assertEquals(expected, NumberTheoreticFunctionsCalculator.symbolKronecker(m + 1, -2))
      assertEquals(-expected, NumberTheoreticFunctionsCalculator.symbolKronecker(m + 3, -2))
      assertEquals(-expected, NumberTheoreticFunctionsCalculator.symbolKronecker(m + 5, -2))
      assertEquals(expected, NumberTheoreticFunctionsCalculator.symbolKronecker(m + 7, -2))
    }
  }

  @Test def testKernel(): Unit = {
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

//  private def negCube(n: Int): Int = n * n * -n

//  private def invalidFunctionF(n: Int): Int = 10

  @Test def testEuclideanGCD(): Unit = {
    println("euclideanGCD")
    assertEquals(1, NumberTheoreticFunctionsCalculator.euclideanGCD(-27, 14))
    assertEquals(3, NumberTheoreticFunctionsCalculator.euclideanGCD(-27, 15))
    assertEquals(9, NumberTheoreticFunctionsCalculator.euclideanGCD(-27, 18))
    assertEquals(1, NumberTheoreticFunctionsCalculator.euclideanGCD(-21L, 13L))
    assertEquals(3, NumberTheoreticFunctionsCalculator.euclideanGCD(-21L, -144L))
    assertEquals(27, NumberTheoreticFunctionsCalculator.euclideanGCD(-27, 0))
    assertEquals(28, NumberTheoreticFunctionsCalculator.euclideanGCD(8400, 8372))
//    try {
//      val result = NumberTheoreticFunctionsCalculator.euclideanGCD(-27, 18, negCube(_))
//      println("gcd(-27, 18) with negative cube function gave this result: " + result)
//      val failMsg = "gcd(-27, 18) with negative cube function should have triggered exception, not given result " + result
//      fail(failMsg)
//    } catch {
//      case iae: IllegalArgumentException => println("gcd(-27, 18) with negative cube function correctly triggered IllegalArgumentException")
//        println("\"" + iae.getMessage + "\"")
//      case e: Exception => val failMsg = e.getClass.getName + " is wrong exception to throw for gcd(-27, 18) with negative cube function"
//        fail(failMsg)
//    }
//    try {
//      val result = NumberTheoreticFunctionsCalculator.euclideanGCD(-27, 18, invalidFunctionF(_))
//      println("gcd(-27, 18) with invalid function F gave this result: " + result)
//      val failMsg = "gcd(-27, 18) with invalid function F should have triggered exception, not given result " + result
//      fail(failMsg)
//    } catch {
//      case nede: NonEuclideanDomainException => println("gcd(-27, 18) with invalid function F correctly triggered NonEuclideanDomainException")
//        println("\"" + nede.getMessage + "\"")
//      case e: Exception => val failMsg = e.getClass.getName + " is wrong exception to throw for gcd(-27, 18) with invalid function F"
//        fail(failMsg)
//    }
  }

  @Test def testEuclideanGCDAlgDegOverflow(): Unit = {
    val a = new ImagQuadInt(3, 7, ringEisenstein, 2)
    val b = new RealQuadInt(5, 8, ringZ2)
    try {
      val gcd = NumberTheoreticFunctionsCalculator.euclideanGCD(a, b)
      val failMsg = "Trying to calculate gcd(" + a.toString + ", " + b.toString + ") should have caused exception, not given result " + gcd.toString
      fail(failMsg)
    } catch {
      case adoe: AlgebraicDegreeOverflowException => println("Trying to calculate gcd(" + a.toString + ", " + b.toString + ") correctly triggered AlgebraicDegreeOverflowException")
        println("\"" + adoe.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is wrong exception to throw for trying to calculate gcd(" + a.toString + ", " + b.toString + ")"
        fail(failMsg)
    }
  }

  @Test def testEuclideanGCDQuadInt(): Unit = {
    var a: QuadInt = new ImagQuadInt(3, 1, ringGaussian)
    var b: QuadInt = new ImagQuadInt(21, -8, ringGaussian)
    var expected: QuadInt = new ImagQuadInt(2, -1, ringGaussian)
    var actual = NumberTheoreticFunctionsCalculator.euclideanGCD(a, b)
    assertEquals(expected, actual)
    a = new ImagQuadInt(19, -1, ringEisenstein, 2)
    b = new ImagQuadInt(7, 1, ringEisenstein)
    expected = new ImagQuadInt(5, -3, ringEisenstein, 2)
    actual = NumberTheoreticFunctionsCalculator.euclideanGCD(a, b)
    assertEquals(expected, actual)
    a = new RealQuadInt(46, 20, ringZ2)
    b = new RealQuadInt(-21, -3, ringZ2)
    expected = new RealQuadInt(7, 1, ringZ2)
    actual = NumberTheoreticFunctionsCalculator.euclideanGCD(a, b)
    assertEquals(expected, actual)
  }

  @Test(timeout = 30000)
  def testEuclideanGCDNonEuclOQi19(): Unit = {
    val ringOQi19 = new ImagQuadRing(-19)
    val primeFactorOf5 = new ImagQuadInt(1, 1, ringOQi19, 2)
    val ten = new ImagQuadInt(10, 0, ringOQi19)
    val actual = NumberTheoreticFunctionsCalculator.euclideanGCD(primeFactorOf5, ten)
    assertEquals(primeFactorOf5, actual)
    val primeFactorOf7 = new ImagQuadInt(3, 1, ringOQi19, 2)
    try {
      val result = NumberTheoreticFunctionsCalculator.euclideanGCD(ten, primeFactorOf7)
      val failMsg = "Trying to calculate gcd(" + ten.toString + ", " + primeFactorOf7.toString + ") by the Euclidean algorithm should have caused exception, not given result " + result.toString
      fail(failMsg)
    } catch {
      case nede: NonEuclideanDomainException => println("Trying to calculate gcd(" + ten.toString + ", " + primeFactorOf7.toString + ") by the Euclidean algorithm correctly triggered NonEuclideanDomainException")
        println("\"" + nede.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is wrong exception to throw for Trying to calculate gcd(" + ten.toString + ", " + primeFactorOf7.toString + ") by the Euclidean algorithm"
        fail(failMsg)
    }
  }

  @Test(timeout = 30000)
  def testEuclideanGCDNonEuclZ14(): Unit = {
    val ringZ14 = new RealQuadRing(14)
    val primeFactorOf13 = new RealQuadInt(1, 1, ringZ14)
    val two = new RealQuadInt(2, 0, ringZ14)
    try {
      val result = NumberTheoreticFunctionsCalculator.euclideanGCD(two, primeFactorOf13)
      val failMsg = "Trying to calculate gcd(" + two.toString + ", " + primeFactorOf13.toString + ") by the Euclidean algorithm should have caused exception, not given result " + result.toString
      fail(failMsg)
    } catch {
      case nede: NonEuclideanDomainException => println("Trying to calculate gcd(" + two.toString + ", " + primeFactorOf13.toString + ") by the Euclidean algorithm correctly triggered NonEuclideanDomainException")
        println("\"" + nede.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is wrong exception to throw for Trying to calculate gcd(" + two.toString + ", " + primeFactorOf13.toString + ") by the Euclidean algorithm"
        fail(failMsg)
    }
  }

  @Test(timeout = 30000)
  def testEuclideanGCDNonEuclOQ69(): Unit = {
    val ringOQ69 = new RealQuadRing(69)
    val primeFactorOf23 = new RealQuadInt(23, 3, ringOQ69, 2)
    val factorOf48 = new RealQuadInt(18, 2, ringOQ69)
    try {
      val result = NumberTheoreticFunctionsCalculator.euclideanGCD(factorOf48, primeFactorOf23)
      val failMsg = "Trying to calculate gcd(" + factorOf48.toString + ", " + primeFactorOf23.toString + ") by the Euclidean algorithm should have caused exception, not given result " + result.toString
      fail(failMsg)
    } catch {
      case nede: NonEuclideanDomainException => println("Trying to calculate gcd(" + factorOf48.toString + ", " + primeFactorOf23.toString + ") by the Euclidean algorithm correctly triggered NonEuclideanDomainException")
        println("\"" + nede.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is wrong exception to throw for Trying to calculate gcd(" + factorOf48.toString + ", " + primeFactorOf23.toString + ") by the Euclidean algorithm"
        fail(failMsg)
    }
    val result = NumberTheoreticFunctionsCalculator.euclideanGCD(primeFactorOf23, factorOf48, NumberTheoreticFunctionsCalculatorTest.adj23Norm)
    assertEquals(1L, Math.abs(result.norm))
  }

  @Test def testFundamentalUnit(): Unit = {
    println("fundamentalUnit")
    var expected = new RealQuadInt(1, 1, ringZ2)
    var actual = NumberTheoreticFunctionsCalculator.fundamentalUnit(ringZ2)
    assertEquals(expected, actual)
    expected = new RealQuadInt(1, 1, ringZPhi, 2)
    actual = NumberTheoreticFunctionsCalculator.fundamentalUnit(ringZPhi)
    assertEquals(expected, actual)
  }

  @Test def testPlaceInPrimarySectorImagQuadInt(): Unit = {
    println("placeInPrimarySector")
    var num = new ImagQuadInt(-1, 6, ringGaussian)
    var expected = new ImagQuadInt(6, 1, ringGaussian)
    var actual = NumberTheoreticFunctionsCalculator.placeInPrimarySector(num)
    assertEquals(expected, actual)
    num = new ImagQuadInt(-17, -7, ringEisenstein, 2)
    expected = new ImagQuadInt(19, -5, ringEisenstein, 2)
    actual = NumberTheoreticFunctionsCalculator.placeInPrimarySector(num)
    assertEquals(expected, actual)
  }

  @Test def testPlaceInPrimarySectorRealQuadInt(): Unit = {
    val num = new RealQuadInt(-5, -2, ringZ2)
    val expected = new RealQuadInt(5, 2, ringZ2)
    val actual = NumberTheoreticFunctionsCalculator.placeInPrimarySector(num)
    assertEquals(expected, actual)
  }

  @Test def testPlaceInPrimarySectorUnsupportedRing(): Unit = {
    val num = new IllDefQuadInt(-8, 3, ringUnsupported)
    try {
      val result = NumberTheoreticFunctionsCalculator.placeInPrimarySector(num)
      val failMsg = "Trying to place " + num.toString + " of ring " + ringUnsupported.toString + " of type " + ringUnsupported.getClass.getName + " should have caused an exception, not given result " + result.toString
      fail(failMsg)
    } catch {
      case unde: UnsupportedNumberDomainException => println("Trying to place " + num.toString + " of ring" + ringUnsupported.toString + " of type " + ringUnsupported.getClass.getName + " correctly triggered UnsupportedNumberDomainException")
        println("\"" + unde.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is the wrong exception to throw for trying to place " + num.toString + " of ring " + ringUnsupported.toString + " of type " + ringUnsupported.getClass.getName
        fail(failMsg)
    }
  }

  @Test def testDivideOutUnits(): Unit = {
    println("divideOutUnits")
    val ring = new RealQuadRing(10)
    val expected = new RealQuadInt(1, 2, ring)
    val unitsMultipliedIn = new RealQuadInt(5281, 1670, ring)
    val actual = NumberTheoreticFunctionsCalculator.divideOutUnits(unitsMultipliedIn)
    assertEquals(expected, actual)
  }

  @Test def testGetOneInRing(): Unit = {
    println("getOneInRing")
    val ring = new ImagQuadRing(-15)
    val expected = new ImagQuadInt(1, 0, ring)
    val actual = NumberTheoreticFunctionsCalculator.getOneInRing(ring)
    assertEquals(expected, actual)
  }

  @Test def testFieldClassNumber(): Unit = {
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
