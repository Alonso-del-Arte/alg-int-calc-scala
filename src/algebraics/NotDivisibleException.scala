package algebraics

import algebraics.quadratics.{ImagQuadInt, ImagQuadRing, RealQuadInt, RealQuadRing}
import fractions.Fraction

class NotDivisibleException(message: String, dividend: AlgInt, divisor: AlgInt,
                            fractions: Array[Fraction])
  extends Exception(message: String) {
  val causingDividend: AlgInt = dividend
  val causingDivisor: AlgInt = divisor
  val causedFractions: Array[Fraction] = fractions
  val pertinentRing: IntRing = dividend.getRing
  private val numValRe = this.pertinentRing match {
    case _: ImagQuadRing => this.causedFractions(0).numericApproximation
    case reR: RealQuadRing => this.causedFractions(0).numericApproximation + this.causedFractions(1).numericApproximation * reR.getRadSqrt
    case _ => Double.MinValue
  }
  private val numValIm = this.pertinentRing match {
    case imR: ImagQuadRing => this.causedFractions(1).numericApproximation * imR.getAbsNegRadSqrt
    case r if r.isPurelyReal => 0.0
    case _ => Double.MinValue
  }

  def this(dividend: AlgInt, divisor: AlgInt, fractions: Array[Fraction]) {
    this(dividend.toString + " is not divisible by " + divisor.toString, dividend, divisor, fractions)
  }

  def getNumericRealPart: Double = this.numValRe

  def getNumericImagPart: Double = this.numValIm

  def getAbs: Double = if (this.pertinentRing.isPurelyReal) {
    Math.abs(this.numValRe)
  } else {
    Math.sqrt(this.numValRe * this.numValRe + this.numValIm * this.numValIm)
  }

  def getBoundingIntegers: List[AlgInt] = {
    this.pertinentRing match {
      case ring: ImagQuadRing =>
        val numerReg = this.causedFractions(0).numericApproximation
        val numerSurd = this.causedFractions(1).numericApproximation
        if (ring.hasHalfIntegers) {
          var topPointA = Math.ceil(numerReg * 2).toInt
          val topPointB = Math.ceil(numerSurd * 2).toInt
          if (Math.abs(topPointA % 2) != Math.abs(topPointB % 2)) topPointA -= 1
          List(new ImagQuadInt(topPointA, topPointB, ring, 2),
            new ImagQuadInt(topPointA - 1, topPointB - 1, ring, 2),
            new ImagQuadInt(topPointA + 1, topPointB - 1, ring, 2),
            new ImagQuadInt(topPointA, topPointB - 2, ring, 2))
        } else {
          val floorA = Math.floor(numerReg).toInt
          val floorB = Math.floor(numerSurd).toInt
          List(new ImagQuadInt(floorA, floorB, ring),
            new ImagQuadInt(floorA + 1, floorB, ring),
            new ImagQuadInt(floorA, floorB + 1, ring),
            new ImagQuadInt(floorA + 1, floorB + 1, ring))
        }
      case _ =>
        val excMsg = "The domain " + this.causingDividend.getRing.toString + " is not supported yet"
        throw new UnsupportedNumberDomainException(excMsg, this.causingDividend.getRing)
    }
  }

  def roundTowardsZero: AlgInt = {
    this.pertinentRing match {
      case ring: RealQuadRing =>
        val floored = Math.floor(this.numValRe).toInt
        new RealQuadInt(floored, 0, ring)
      case _: ImagQuadRing =>
        val bounds = this.getBoundingIntegers
        var currAbs = bounds.head.abs
        var closestSoFar = currAbs
        var currIndex = 1
        var bestIndex = 0
        while (currIndex < bounds.length) {
          currAbs = bounds(currIndex).abs
          if (currAbs < closestSoFar) {
            closestSoFar = currAbs
            bestIndex = currIndex
          }
          currIndex += 1
        }
        bounds(bestIndex)
      case _ =>
        val excMsg = "The domain " + this.causingDividend.getRing.toString + " is not supported"
        throw new UnsupportedNumberDomainException(excMsg, this.causingDividend.getRing)}
  }

  def roundAwayFromZero: AlgInt = this.causingDividend

}
