package calculators

import algebraics.{AlgInt, AlgebraicDegreeOverflowException, IntRing, NonEuclideanDomainException, NotDivisibleException, UnsupportedNumberDomainException}
import algebraics.quadratics.{ImagQuadInt, ImagQuadRing, QuadInt, QuadRing, RealQuadInt, RealQuadRing}
import algebraics.unary.{UnaryInt, Z}

object NumberTheoreticFunctionsCalculator {

  val GAUSSIAN_RING = new ImagQuadRing(-1)
  val EISENSTEIN_RING = new ImagQuadRing(-3)

  val IMAGINARY_UNIT = new ImagQuadInt(0, 1, GAUSSIAN_RING)
  val OMEGA = new ImagQuadInt(-1, 1, EISENSTEIN_RING, 2)

  val DEGREES_45_IN_RADIANS = 0.7853981633974483
  val DEGREES_30_IN_RADIANS = 0.5235987755982988730771

  def primeFactors(num: Int): Vector[Int] = {
    var n = Math.abs(num)
    val factorList = scala.collection.mutable.ArrayBuffer.empty[Int]
    if (n == 0) {
      factorList += 0
    } else {
      if (num < 0) factorList += -1
      while (n % 2 == 0) {
        factorList += 2
        n /= 2
      }
      var i = 3
      while (i <= n) {
        while (n % i == 0) {
          factorList += i
          n /= i
        }
        i += 2
      }
    }
    factorList.toVector
  }

  // STUB TO FAIL THE TEST
  def primeFactors(num: AlgInt): Vector[AlgInt] = Vector(new RealQuadInt(0, 0, new RealQuadRing(2)))

  def isPrime(num: Long): Boolean = Math.abs(num) match {
    case 0 => false
    case 1 => false
    case n => (2 to Math.floor(Math.sqrt(n)).toInt) forall (p => n % p != 0)
  }

  // STUB
  def isPrime(num: AlgInt): Boolean = false

  // STUB
  def isIrreducible(num: AlgInt): Boolean = false

  def isDivisibleBy(a: AlgInt, b: AlgInt): Boolean = if (a.getRing == b.getRing) {
    a.getRing match {
      case r: QuadRing => try {
        a.asInstanceOf[QuadInt] / b.asInstanceOf[QuadInt]
        true
      } catch {
        case _: NotDivisibleException => false
      }
      case _ => val excMsg = "The ring " + a.getRing.toString + " is not supported for this operation"
        throw new UnsupportedNumberDomainException(excMsg, a.getRing)
    }
  } else {
    val excMsg = a.toString + " is from " + a.getRing.toString + " but " + b.toString + " is from " + b.getRing.toString
    throw new AlgebraicDegreeOverflowException(excMsg, a, b)
  }

  def isSquarefree(num: Int): Boolean = num match {
    case -1 => true
    case 0 => false
    case 1 => true
    case _ => if (num % 4 == 0) {
      false
    } else {
      var noDupFactorFound = true
      val threshold = Math.sqrt(Math.abs(num))
      var currRoot = 3
      do {
        val currSquare = currRoot * currRoot
        noDupFactorFound = num % currSquare != 0
        currRoot += 2
      } while (noDupFactorFound && currRoot <= threshold)
      noDupFactorFound
    }
  }

  def randomSquarefreeNumber(bound: Int): Int = {
    val threshold = if (bound < 0) {
      -bound
    } else {
      bound
    }
    var rand: Int = Math.floor(Math.random * threshold).asInstanceOf[Int] + 1
    while (!isSquarefree(rand)) {
      rand += 1
    }
    if (rand > threshold) rand = 1
    rand
  }

  def moebiusMu(num: Int): Byte = 0

  def symbolLegendre(a: Int, p: Int): Byte = {
    if (p == -2 || p == 2 || !isPrime(p)) {
      val excMsg = p.toString + " is not an odd prime. Consider using the Jacobi or Kronecker symbol instead."
      throw new IllegalArgumentException(excMsg)
    }
    val oddPrime = Math.abs(p)
    val exponent = (oddPrime - 1) / 2
    val modStop = oddPrime - 2
    var adjA = if (a > oddPrime - 1) a % oddPrime else a
    if (adjA == oddPrime - 1) adjA = -1
    while (adjA < -1) adjA += oddPrime
    var power = adjA
    var currExpo = 1
    while (currExpo < exponent) {
      power *= adjA
      while (power > modStop) power -= oddPrime
      while (power < -1) power += oddPrime
      currExpo += 1
    }
    power.toByte
  }

  def symbolJacobi(n: Int, m: Int): Byte = {
    val factors = primeFactors(m)
    val symbols = factors.map(symbolLegendre(n, _))
    symbols.product
  }

  private def symbolKroneckerNegOne(n: Int): Byte = 0

  private def symbolKroneckerNegTwo(n: Int): Byte = 0

  def symbolKronecker(n: Int, m: Int): Byte = 0

  def kernel(num: Int): Int = 0

  val normWrap: AlgInt => Long = {
    case num: RealQuadInt => Math.abs(num.norm)
    case number => number.norm
  }

  // TODO: Rewrite tests
  def euclideanGCD(a: Long, b: Long,
                   norm: Long => Long = Math.abs): Long = {
//    val phi = norm(b)
    Math.abs(a)
  }
  /* match {
    case 0 => a;
    case _ => Math.abs(euclideanGCD(b, a % b)) */
//  }

//  def euclideanGCD(a: AlgInt, b: AlgInt, eucFn: AlgInt => Long = normWrap): AlgInt = {
//    if (a.getRing != b.getRing) {
//      val excMsg = "GCD(" + a.toString + ", " + b.toString + ") is in ring of higher degree"
//      throw new AlgebraicDegreeOverflowException(excMsg, a, b)
//    }
//    a.getRing match {
//      case ring: QuadRing => var currA = a.asInstanceOf[QuadInt]
//        var currB = b.asInstanceOf[QuadInt]
//        if (eucFn(a) < eucFn(b)) {
//          val swapper = currA
//          currA = currB
//          currB = swapper
//        }
//        val zero = QuadInt(0, 0, ring)
//        var currRemainder = zero
//        while (currB != zero) {
//          try {
//            currA / currB
//            currRemainder = zero
//          } catch {
//            case nde: NotDivisibleException => val roundDiv = nde.roundTowardsZero.asInstanceOf[QuadInt]
//              val tempMultiple = roundDiv * currB
//              currRemainder = currA - tempMultiple
//          }
//          val phiB = eucFn(currB)
//          val phiRem = eucFn(currRemainder)
//          if (phiB <= phiRem) {
//            val excMsg = ring.toString + " is not Euclidean for provided function since f(" + currB.toString + ") = " + phiB.toString + " but f(" + currRemainder.toString + ") = " + phiRem.toString
//            throw new NonEuclideanDomainException(excMsg, currB, currRemainder, eucFn)
//          }
//          currA = currB
//          currB = currRemainder
//        }
//        currA
//      case _ => val excMsg = "The domain " + a.getRing.toString + " is not yet supported for the GCD operation"
//        throw new UnsupportedNumberDomainException(excMsg, a.getRing)
//    }
//  }

  def fundamentalUnit(ring: IntRing): AlgInt = new UnaryInt(0)

  private def turnNumber(num: ImagQuadInt, turnUnit: ImagQuadInt, maxAngle: Double): QuadInt = {
    var turning: QuadInt = num
    while (turning.angle < -maxAngle || turning.angle > maxAngle) {
      turning = turning * turnUnit
    }
    turning
  }

  private def placeImagQuadIntInPrimarySector(num: ImagQuadInt): QuadInt = {
    num.getRing.asInstanceOf[QuadRing].radicand match {
      case -1 => turnNumber(num, IMAGINARY_UNIT, DEGREES_45_IN_RADIANS)
      case -3 => turnNumber(num, OMEGA, DEGREES_30_IN_RADIANS)
      case _ => if (num.regPart < 0) {
        num * -1
      } else {
        num
      }
    }
  }

  def placeInPrimarySector(num: AlgInt): AlgInt = num match {
    case num: ImagQuadInt => placeImagQuadIntInPrimarySector(num)
    case num: RealQuadInt => if (num.getRealPartNumeric < 0) {
      num * -1
    } else {
      num
    }
    case _ => val failMsg = "The ring of " + num.toString + " is not yet supported"
      throw new UnsupportedNumberDomainException(failMsg, num.getRing)
  }

  def divideOutUnits(num: AlgInt): AlgInt = if (num.norm == 0) {
    num
  } else {
    num match {
      case n: ImagQuadInt => placeImagQuadIntInPrimarySector(n)
      case _ => val failMsg = "The ring of " + num.toString + " is not yet supported for divide out units operation"
        throw new UnsupportedNumberDomainException(failMsg, num.getRing)
    }
  }

  def getOneInRing(ring: IntRing): AlgInt = ring match {
    case Z => new UnaryInt(1)
    case r: QuadRing => QuadInt(1, 0, r)
    case _ => val excMsg = ring.toString + " is not yet supported"
      throw new UnsupportedNumberDomainException(excMsg, ring)
  }

  private def w(d: Int): Short = 0

  def fieldClassNumber(ring: IntRing): Int = 0

}
