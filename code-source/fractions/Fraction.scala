package fractions

import calculators.NumberTheoreticFunctionsCalculator

object Fraction {

  val HASH_SEP = 65536

  import scala.language.implicitConversions

  implicit def IntToFraction(n: Int): Fraction = new Fraction(n)

  def inferStep(startFract: Fraction, endFract: Fraction): Fraction = {
    if (startFract.denominator == endFract.denominator) {
      new Fraction(1, startFract.denominator)
    } else {
      val diff = endFract - startFract
      val inferredStep = new Fraction(1, diff.denominator)
      if (endFract > startFract) {
        inferredStep
      } else {
        -inferredStep
      }
    }
  }

}

class Fraction(numer: Long, denom: Long = 1L) extends Ordered[Fraction] {
  if (denom == 0) {
    throw new IllegalArgumentException("Denominator 0 is not allowed")
  }
  var gcdNumDen: Long = NumberTheoreticFunctionsCalculator.euclideanGCD(numer, denom)
  if (denom < 0) {
    gcdNumDen *= -1
  }
  val numerator: Long = numer / gcdNumDen
  val denominator: Long = denom / gcdNumDen

  override def toString: String = if (this.denominator == 1) {
    this.numerator.toString
  } else {
    this.numerator + "/" + this.denominator
  }

  override def equals(obj: Any): Boolean = obj match {
    case obj: Fraction => this.numerator == obj.numerator && this.denominator == obj.denominator
    case _ => false
  }

  override def hashCode: Int = {
    var hash: Int = this.denominator.asInstanceOf[Int] % Fraction.HASH_SEP
    hash *= Fraction.HASH_SEP
    hash + (this.numerator.asInstanceOf[Int] % Fraction.HASH_SEP)
  }

  def +(addend: Fraction): Fraction = {
    val interNumerA = this.numerator * addend.denominator
    val interNumerB = addend.numerator * this.denominator
    val sumNumer = interNumerA + interNumerB
    val sumDenom = this.denominator * addend.denominator
    new Fraction(sumNumer, sumDenom)
  }

  def unary_- = new Fraction(-this.numerator, this.denominator)

  def -(subtrahend: Fraction): Fraction = this + (-subtrahend)

  def *(multiplicand: Fraction): Fraction = {
    val multNumer = this.numerator * multiplicand.numerator
    val multDenom = this.denominator * multiplicand.denominator
    new Fraction(multNumer, multDenom)
  }

  def /(divisor: Fraction): Fraction = {
    val divNumer = this.numerator * divisor.denominator
    val divDenom = this.denominator * divisor.numerator
    new Fraction(divNumer, divDenom)
  }

  def reciprocal: Fraction = {
    new Fraction(this.denominator, this.numerator)
  }

  def numericApproximation: Double = this.numerator.asInstanceOf[Double] / this.denominator.asInstanceOf[Double]

  def to(end: Fraction): FractionRange = {
    new FractionRange(this, end)
  }

  override def compare(that: Fraction): Int = {
    val diff = this - that
    diff.numerator match {
      case 0 => 0
      case n if n < 0 => -1
      case _ => 1
    }
  }

}
