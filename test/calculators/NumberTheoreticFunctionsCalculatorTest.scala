package calculators

import algebraics.{AlgInt, AlgebraicDegreeOverflowException,
  NonEuclideanDomainException, UnsupportedNumberDomainException}
import algebraics.quadratics.{ImagQuadInt, ImagQuadRing, QuadInt, RealQuadInt,
  RealQuadRing}

import calculators.NumberTheoreticFunctionsCalculator._

import java.time.Duration

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

object NumberTheoreticFunctionsCalculatorTest {

  def adj23Norm(num: AlgInt): Long = {
    var interim = Math.abs(num.norm)
    while (euclideanGCD(interim, 23L) > 1
        && interim > 0) {
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

  @Test def testPrimeFactors(): Unit = {
    println("primeFactors")
    val expected = Vector[Int](-1, 2, 7)
    val actual = primeFactors(-14)
    assertEquals(expected, actual)
  }

  @Test def testPrimeFactorsAlgInt(): Unit = {
    val negOne = new RealQuadInt(-1, 0, ringZ2)
    val sqrt2 = new RealQuadInt(0, 1, ringZ2)
    val norm7 = new RealQuadInt(3, 1, ringZ2)
    val toBeFactored = new RealQuadInt(-12, -11, ringZ2)
    val expected = Vector[RealQuadInt](negOne, sqrt2, norm7, norm7)
    val actual = primeFactors(toBeFactored)
    assertEquals(expected, actual)
  }

  @Test def testIsPrime(): Unit = {
    println("isPrime")
    var number = 7
    var msg = number.toString + " should be found to be prime"
    assert(isPrime(number), msg)
    number = 8
    msg = number.toString + " should not be found to be prime"
    assert(!isPrime(number), msg)
    var longNumber = 13L
    msg = longNumber.toString + " should be found to be prime"
    assert(isPrime(longNumber), msg)
    longNumber = 14L
    msg = longNumber.toString + " should not be found to be prime"
    assert(!isPrime(longNumber), msg)
    for (i <- 0 until maxPrimePi) {
      val currPrime = primeSieve(i)
      msg = currPrime.toString + " should be found to be prime"
      assert(isPrime(currPrime), msg)
      val currComposite = currPrime * 210
      msg = currComposite.toString + " should not be found to be prime"
      assert(!isPrime(currComposite), msg)
    }
    // TODO: Write test for AlgInt
  }

  @Test def testIsIrreducible(): Unit = {
    println("isIrreducible")
    val ringZi5 = new ImagQuadRing(-5)
    var num = new ImagQuadInt(1, 1, ringZi5)
    var msg = num.toString + " should be found to be irreducible"
    assert(isIrreducible(num), msg)
    num = new ImagQuadInt(1, 7, ringZi5)
    msg = num.toString + " should not be found to be irreducible"
    assert(!isIrreducible(num), msg)
  }

  @Test def testIsDivisibleBy(): Unit = {
    println("isDivisibleBy")
    val testDividend = new RealQuadInt(59, 0, ringZPhi)
    val testDivisor = new RealQuadInt(8, 1, ringZPhi)
    var msg = testDividend.toString + " should be found to be divisible by " +
      testDivisor.toString
    assert(NumberTheoreticFunctionsCalculator
      .isDivisibleBy(testDividend, testDivisor), msg)
    msg = testDivisor.toString + " should not be found to be divisible by " +
      testDividend.toString
    assert(!NumberTheoreticFunctionsCalculator
      .isDivisibleBy(testDivisor, testDividend), msg)
  }

  // TODO: Split into smaller tests
  @Test def testIsSquarefree(): Unit = {
    println("isSquarefree")
    var msg = "The number -1 should be found to be squarefree"
    assert(isSquarefree(-1), msg)
    msg = "The number 0 should not be found to be squarefree"
    assert(isSquarefree(0), msg)
    msg = "The number 1 should be found to be squarefree"
    assert(isSquarefree(1), msg)
    for (pIndex <- 0 to (maxPrimePi - 2)) {
      val p = primeSieve(pIndex)
      msg = "The number " + p.toString + " should be found to be squarefree"
      assert(isSquarefree(p), msg)
      val q = -primeSieve(pIndex + 1)
      val pq = p * q
      msg = "The number " + pq.toString + " should be found to be squarefree"
      assert(isSquarefree(pq), msg)
      val pSquared = p * p
      msg = "The number " + pSquared.toString +
        " should not be found to be squarefree"
      assert(!isSquarefree(pSquared), msg)
      if (p < 1290) {
      val pCubed = p * pSquared
      msg = "The number " + pCubed.toString +
        " should not be found to be squarefree"
      assert(isSquarefree(pCubed), msg)
      if (p < 220) {
        val pCubedTimesQ = pCubed * q
        msg = "The number " + pCubedTimesQ.toString +
          " should not be found to be squarefree"
        assert(!isSquarefree(pCubedTimesQ), msg)
      }}
    }
  }

  @Test def testRandomSquarefreeNumber(): Unit = {
    println("randomSquarefreeNumber")
    val specifiedBound = 65536
    val randomSquarefree = NumberTheoreticFunctionsCalculator
      .randomSquarefreeNumber(specifiedBound)
    println("Function came up with this potentially squarefree random number: "
      + randomSquarefree.toString)
    var msg = "Number said to be squarefree should not be divisible by 4"
    assert(randomSquarefree % 4 != 0, msg)
    val squareRootFloor = Math.floor(Math.sqrt(randomSquarefree)).asInstanceOf[Int]
    for (i <- 3 to squareRootFloor by 2) {
      val square = i * i
      msg = "Number said to be squarefree should not be divisible by " +
        square.toString
      assert(randomSquarefree % square != 0, msg)
    }
    msg = "Random number should not exceed specified bound of " +
      specifiedBound.toString
    assert(randomSquarefree <= specifiedBound, msg)
  }

  @Test def testMoebiusMu(): Unit = {
    println("moebiusMu")
    assertEquals(-1, moebiusMu(19))
    assertEquals(0, moebiusMu(20))
    assertEquals(1, moebiusMu(21))
    for (pIndex <- 0 to (maxPrimePi - 2)) {
      val currPrime = primeSieve(pIndex)
      assertEquals(-1, moebiusMu(currPrime))
      val squaredPrime = currPrime * currPrime
      assertEquals(0, NumberTheoreticFunctionsCalculator
        .moebiusMu(squaredPrime))
      val nextPrime = primeSieve(pIndex + 1)
      val semiPrime = currPrime * nextPrime
      assertEquals(1, moebiusMu(semiPrime))
    }
  }

  @Test def testLegendreSymbolP7Cases(): Unit = {
    assertEquals(-1, symbolLegendre(6, 7))
    assertEquals(0, symbolLegendre(7, 7))
    assertEquals(1, symbolLegendre(8, 7))
  }

  @Test def testLegendreSymbol(): Unit = {
    println("symbolLegendre")
    val quadRecipMsg = "Quadratic reciprocity applies: "
    for {pIndex <- 1 until primePi1K
         qIndex <- (pIndex + 1) to primePi1K} {
      val p = primeSieve(pIndex)
      val q = primeSieve(qIndex)
      assertEquals(0, symbolLegendre(p, p))
      (p % 4, q % 4) match {
        case (3, 3) => val msg = s"($p, $q) = -($q, $p)"
          assertNotEquals(NumberTheoreticFunctionsCalculator
            .symbolLegendre(p, q), NumberTheoreticFunctionsCalculator
            .symbolLegendre(q, p), msg)
        case _ => val msg = s"$quadRecipMsg($p, $q) = ($q, $p)"
          assertEquals(symbolLegendre(p, q),
            symbolLegendre(q, p), msg)
      }
    }
  }

  @Test def testLegendreSymbolBadArgEvenPrime(): Unit = {
    try {
      val result = symbolLegendre(7, 2)
      fail("Legendre(7, 2) should not have given result " + result.toString)
    } catch {
      case iae: IllegalArgumentException =>
        println("Legendre(7, 2) correctly triggered IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
    }
  }

  @Test def testLegendreSymbolBadArgs(): Unit = {
    try {
      val result = symbolLegendre(12, 35)
      fail("Legendre(12, 35) should not have given result " + result.toString)
    } catch {
      case iae: IllegalArgumentException =>
        println("Legendre(12, 35) correctly triggered IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
    }
  }

  @Test def testJacobiSymbolM35Cases(): Unit = {
    assertEquals(-1, symbolJacobi(104, 35))
    assertEquals(0, symbolJacobi(105, 35))
    assertEquals(1, symbolJacobi(106, 35))
  }

  @Test def testJacobiLegendreCorrespondence(): Unit = {
    for {pIndex <- 1 until primePi1K
         qIndex <- pIndex until primePi1K} {
      val p = primeSieve(pIndex)
      val q = primeSieve(qIndex)
      val msg = s"Jacobi($p, $q) should be equal to Legendre($p, $q)"
      val expected = symbolLegendre(p, q)
      val actual = symbolJacobi(p, q)
      assertEquals(expected, actual, msg)
    }
    println("Jacobi-Legendre correspondence checks out...")
  }

  @Test def testJacobiSymbol(): Unit = {
    println("symbolJacobi")
    for {pIndex <- 1 until primePi1K
         qIndex <- pIndex until primePi1K} {
      val p = primeSieve(pIndex)
      val q = primeSieve(qIndex)
      val m = p * q
      for (n <- 15 to 20) {
        val expected = symbolLegendre(n, p) *
          symbolLegendre(n, q)
        val actual = symbolJacobi(n, m)
        assertEquals(expected, actual)
      }
    }
  }

  @Test def testJacobiSymbolBadArgEvenM(): Unit = {
    try {
      val result = symbolJacobi(7, 2)
      val msg = "Jacobi(7, 2) should not have given result " + result.toString
      fail(msg)
    } catch {
      case iae: IllegalArgumentException =>
        println("Jacobi(7, 2) correctly caused IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
      case e: Exception =>
        val msg = e.getClass.getName +
          " is the wrong exception to throw for calling Jacobi(7, 2)"
        fail(msg)
    }
  }

  @Test def testJacobiSymbolBadArgNegM(): Unit = {
    try {
      val result = symbolJacobi(7, -3)
      val msg = "Jacobi(7, -3) should not have given result " + result.toString
      fail(msg)
    } catch {
      case iae: IllegalArgumentException =>
        println("Jacobi(7, -3) correctly caused IllegalArgumentException")
        println("\"" + iae.getMessage + "\"")
      case e: Exception =>
        val msg = e.getClass.getName +
          " is the wrong exception to throw for calling Jacobi(7, -3)"
        fail(msg)
    }
  }

  @Test def testKroneckerSymbolM70CasesWNegN(): Unit = {
    assertEquals(-1, symbolKronecker(-33, 70))
    assertEquals(0, symbolKronecker(-32, 70))
    assertEquals(1, symbolKronecker(-31, 70))
  }

  @Test def testKroneckerSymbolM70Cases(): Unit = {
    assertEquals(-1, symbolKronecker(31, 70))
    assertEquals(0, symbolKronecker(32, 70))
    assertEquals(1, symbolKronecker(33, 70))
  }

  @Test def testKroneckerLegendreCorrespondence(): Unit = {
    for {pIndex <- 1 until primePi1K
         qIndex <- pIndex until primePi1K} {
      val p = primeSieve(pIndex)
      val q = primeSieve(qIndex)
      val msg = s"Kronecker($p, $q) should be equal to Legendre($p, $q)"
      val expected = symbolLegendre(p, q)
      val actual = symbolKronecker(p, q)
      assertEquals(expected, actual, msg)
    }
    println("Kronecker-Legendre correspondence checks out...")
  }

  @Test def testKroneckerJacobiCorrespondence(): Unit = {
    for {j <- -10 to 10
         b <- 5 to 15 by 2} {
      val expected = symbolJacobi(j, b)
      val actual = symbolKronecker(j, b)
      assertEquals(expected, actual)
    }
    println("Kronecker-Jacobi correspondence checks out...")
  }

  @Test def testKroneckerSymbol(): Unit = {
    println("symbolKronecker")
    for (m <- -24 to 24 by 8) {
      assertEquals(-1, symbolKronecker(m + 3, 2))
      assertEquals(-1, symbolKronecker(m + 5, 2))
      assertEquals(0, symbolKronecker(m, 2))
      assertEquals(0, symbolKronecker(m + 2, 2))
      assertEquals(0, symbolKronecker(m + 4, 2))
      assertEquals(0, symbolKronecker(m + 6, 2))
      assertEquals(1, symbolKronecker(m + 1, 2))
      assertEquals(1, symbolKronecker(m + 7, 2))
      val expected = Integer.signum(m + 1)
      assertEquals(expected, symbolKronecker(m + 1, -2))
      assertEquals(-expected, symbolKronecker(m + 3, -2))
      assertEquals(-expected, symbolKronecker(m + 5, -2))
      assertEquals(expected, symbolKronecker(m + 7, -2))
    }
  }

  @Test def testKernel(): Unit = {
    println("kernel")
    val expected = 14
    var num = 14
    var actual = kernel(num)
    assertEquals(expected, actual)
    num *= 2
    actual = kernel(num)
    assertEquals(expected, actual)
    num *= 7
    actual = kernel(num)
    assertEquals(expected, actual)
  }

//  private def negCube(n: Int): Int = n * n * -n

//  private def invalidFunctionF(n: Int): Int = 10

  @Test def testEuclideanGCD(): Unit = {
    println("euclideanGCD")
    fail("REWRITE THIS TEST")
    assertEquals(1, euclideanGCD(-27, 14))
    assertEquals(3, euclideanGCD(-27, 15))
    assertEquals(9, euclideanGCD(-27, 18))
    assertEquals(1, euclideanGCD(-21L, 13L))
    assertEquals(3, euclideanGCD(-21L, -144L))
    assertEquals(27, euclideanGCD(-27, 0))
    assertEquals(28, euclideanGCD(8400, 8372))
  }

  @Test def testEuclideanGCDAlgDegOverflow(): Unit = {
    fail("REWRITE THIS TEST")
//    val a = new ImagQuadInt(3, 7, ringEisenstein, 2)
//    val b = new RealQuadInt(5, 8, ringZ2)
//    try {
//      val gcd = euclideanGCD(a, b)
//      val msg =
//        s"Trying to calculate gcd($a, $b) should not have given result $gcd"
//      fail(msg)
//    } catch {
//      case adoe: AlgebraicDegreeOverflowException => println("Trying to calculate gcd(" +
//        a.toString + ", " + b.toString +
//        ") correctly triggered AlgebraicDegreeOverflowException")
//        println("\"" + adoe.getMessage + "\"")
//      case e: Exception => val msg = e.getClass.getName +
//        " is wrong exception to throw for trying to calculate gcd(" +
//        a.toString + ", " + b.toString + ")"
//        fail(msg)
//    }
  }

  @Test def testEuclideanGCDQuadInt(): Unit = {
    fail("REWRITE THIS TEST")
//    var a: QuadInt = new ImagQuadInt(3, 1, ringGaussian)
//    var b: QuadInt = new ImagQuadInt(21, -8, ringGaussian)
//    var expected: QuadInt = new ImagQuadInt(2, -1, ringGaussian)
//    var actual = euclideanGCD(a, b)
//    assertEquals(expected, actual)
//    a = new ImagQuadInt(19, -1, ringEisenstein, 2)
//    b = new ImagQuadInt(7, 1, ringEisenstein)
//    expected = new ImagQuadInt(5, -3, ringEisenstein, 2)
//    actual = euclideanGCD(a, b)
//    assertEquals(expected, actual)
//    a = new RealQuadInt(46, 20, ringZ2)
//    b = new RealQuadInt(-21, -3, ringZ2)
//    expected = new RealQuadInt(7, 1, ringZ2)
//    actual = euclideanGCD(a, b)
//    assertEquals(expected, actual)
  }

  @Test
  def testEuclideanGCDNonEuclOQi19(): Unit = {
    fail("REWRITE THIS TEST")
//    val ringOQi19 = new ImagQuadRing(-19)
//    val primeFactorOf5 = new ImagQuadInt(1, 1, ringOQi19, 2)
//    val ten = new ImagQuadInt(10, 0, ringOQi19)
//    val actual = euclideanGCD(primeFactorOf5, ten)
//    assertEquals(primeFactorOf5, actual)
//    val primeFactorOf7 = new ImagQuadInt(3, 1, ringOQi19, 2)
//    try {
//      val result = euclideanGCD(ten, primeFactorOf7)
//      val failMsg = "Trying to calculate gcd(" + ten.toString + ", " + primeFactorOf7.toString + ") by the Euclidean algorithm should have caused exception, not given result " + result.toString
//      fail(failMsg)
//    } catch {
//      case nede: NonEuclideanDomainException => println("Trying to calculate gcd(" + ten.toString + ", " + primeFactorOf7.toString + ") by the Euclidean algorithm correctly triggered NonEuclideanDomainException")
//        println("\"" + nede.getMessage + "\"")
//      case e: Exception => val failMsg = e.getClass.getName + " is wrong exception to throw for Trying to calculate gcd(" + ten.toString + ", " + primeFactorOf7.toString + ") by the Euclidean algorithm"
//        fail(failMsg)
//    }
  }

  @Test
  def testEuclideanGCDNonEuclZ14(): Unit = {
    fail("REWRITE THIS TEST")
//    val ringZ14 = new RealQuadRing(14)
//    val primeFactorOf13 = new RealQuadInt(1, 1, ringZ14)
//    val two = new RealQuadInt(2, 0, ringZ14)
//    assertTimeoutPreemptively[Unit](Duration.ofSeconds(30), () => {
//      val exc = assertThrows(classOf[NonEuclideanDomainException], () => {
//        val result = euclideanGCD(two, primeFactorOf13)
//        println(s"gcd($two, $primeFactorOf13) said to be $result?")
//      })
//      val excMsg = exc.getMessage
//      assert(excMsg != null, "Message should not be null")
//      println("\"" + excMsg + "\"")
//    })
  }

  // TODO: Break up into two separate tests
  @Test
  def testEuclideanGCDNonEuclOQ69(): Unit = {
    fail("REWRITE THIS TEST")
//    val ringOQ69 = new RealQuadRing(69)
//    val primeFactorOf23 = new RealQuadInt(23, 3, ringOQ69, 2)
//    val factorOf48 = new RealQuadInt(18, 2, ringOQ69)
//    val result = NumberTheoreticFunctionsCalculator
//      .euclideanGCD(primeFactorOf23, factorOf48,
//        NumberTheoreticFunctionsCalculatorTest.adj23Norm).norm
//    assertEquals(1L, Math.abs(result))
//    assertTimeoutPreemptively[Unit](Duration.ofSeconds(30), () => {
//      val exc = assertThrows(classOf[NonEuclideanDomainException], () => {
//        val unexpectedResult = euclideanGCD(primeFactorOf23, factorOf48)
//        println(s"gcd($primeFactorOf23, $factorOf48) said to be $unexpectedResult?")
//      })
//      val excMsg = exc.getMessage
//      assert(excMsg != null, "Message should not be null")
//      println("\"" + excMsg + "\"")
//    })
  }

  @Test def testFundamentalUnit(): Unit = {
    println("fundamentalUnit")
    var expected = new RealQuadInt(1, 1, ringZ2)
    var actual = fundamentalUnit(ringZ2)
    assertEquals(expected, actual)
    expected = new RealQuadInt(1, 1, ringZPhi, 2)
    actual = fundamentalUnit(ringZPhi)
    assertEquals(expected, actual)
  }

  @Test def testPlaceInPrimarySectorImagQuadInt(): Unit = {
    println("placeInPrimarySector")
    fail("REWRITE THIS TEST")
    var num = new ImagQuadInt(-1, 6, ringGaussian)
    var expected = new ImagQuadInt(6, 1, ringGaussian)
    var actual = placeInPrimarySector(num)
    assertEquals(expected, actual)
    num = new ImagQuadInt(-17, -7, ringEisenstein, 2)
    expected = new ImagQuadInt(19, -5, ringEisenstein, 2)
    actual = placeInPrimarySector(num)
    assertEquals(expected, actual)
  }

  @Test def testPlaceInPrimarySectorRealQuadInt(): Unit = {
    val num = new RealQuadInt(-5, -2, ringZ2)
    val expected = new RealQuadInt(5, 2, ringZ2)
    val actual = placeInPrimarySector(num)
    assertEquals(expected, actual)
  }

  @Test def testPlaceInPrimarySectorUnsupportedRing(): Unit = {
    fail("REWRITE THIS TEST")
//    val num = new IllDefQuadInt(-8, 3, ringUnsupported)
//    val exc = assertThrows(classOf[UnsupportedNumberDomainException], () => {
//      val result = placeInPrimarySector(num)
//      println("Trying to place " + num.toString + " of ring "
//        + ringUnsupported.toString + " of type "
//        + ringUnsupported.getClass.getName
//        + " should have caused an exception, not given result "
//        + result.toString)
//    })
//    val excMsg = exc.getMessage
//    assert(excMsg != null, "Message should not be null")
//    println("\"" + excMsg + "\"")
  }

  @Test def testDivideOutUnits(): Unit = {
    println("divideOutUnits")
    fail("REWRITE THIS TEST")
    val ring = new RealQuadRing(10)
    val expected = new RealQuadInt(1, 2, ring)
    val unitsMultipliedIn = new RealQuadInt(5281, 1670, ring)
    val actual = NumberTheoreticFunctionsCalculator
      .divideOutUnits(unitsMultipliedIn)
    assertEquals(expected, actual)
  }

  @Test def testGetOneInRing(): Unit = {
    println("getOneInRing")
    val ring = new ImagQuadRing(-15)
    val expected = new ImagQuadInt(1, 0, ring)
    val actual = getOneInRing(ring)
    assertEquals(expected, actual)
  }

  @Test def testFieldClassNumber(): Unit = {
    println("fieldClassNumber")
    val numbersHeegner = Array(-1, -2, -3, -7, -11, -19, -43, -67, -163)
    for (d <- numbersHeegner) {
      val imagUFD = new ImagQuadRing(d)
      assertEquals(1, NumberTheoreticFunctionsCalculator
        .fieldClassNumber(imagUFD))
      val imagNonUFD = new ImagQuadRing(5 * d)
      assert(fieldClassNumber(imagNonUFD) > 1)
    }
    val selRealUFDDiscrs = Array(2, 3, 6, 7, 11, 13, 17, 19, 21, 22, 23, 29)
    for (dR <- selRealUFDDiscrs) {
      val realUFD = new RealQuadRing(dR)
      assertEquals(1, NumberTheoreticFunctionsCalculator
        .fieldClassNumber(realUFD))
      val realNonUFD = new RealQuadRing(5 * dR)
      assert(fieldClassNumber(realNonUFD) > 1)
    }
  }

}
