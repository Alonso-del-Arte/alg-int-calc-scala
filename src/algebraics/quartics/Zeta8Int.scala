package algebraics.quartics

import algebraics.{AlgebraicDegreeOverflowException, IntRing, NotDivisibleException, UnsupportedNumberDomainException}
import algebraics.quadratics.{ImagQuadInt, ImagQuadRing, QuadInt, RealQuadInt, RealQuadRing}
import fractions.Fraction

object Zeta8Int {

//  private val SQRT_ONE_HALF = Math.sqrt(2)/2

  import scala.language.implicitConversions

  implicit def IntToZeta8Int(n: Int): Zeta8Int = new Zeta8Int(n, 0, 0, 0)

  def convertFromQuadInt(quadInt: QuadInt): Zeta8Int = quadInt.ring.radicand match {
    case -2 => new Zeta8Int(quadInt.regPart, quadInt.surdPart, 0, quadInt.surdPart)
    case -1 => new Zeta8Int(quadInt.regPart, 0, quadInt.surdPart, 0)
    case 2 => new Zeta8Int(quadInt.regPart, quadInt.surdPart, 0, -quadInt.surdPart)
    case _ => if (quadInt.surdPart == 0) {
      new Zeta8Int(quadInt.regPart, 0, 0, 0)
    } else {
      val excMsg = "The ring " + quadInt.getRing.toString + " is not supported for this operation"
      throw new UnsupportedNumberDomainException(excMsg, quadInt.getRing)
    }
  }

}

class Zeta8Int(a: Int, b: Int, c: Int, d: Int) extends QuartInt {
  val realIntPart: Int = a
  val zeta8Part: Int = b
  val imagIntPart: Int = c
  val zeta8CuPart: Int = d
  private[this] val numValRe: Double = 0.0
  private[this] val numValIm: Double = 0.0

  override def algebraicDegree: Int = (this.realIntPart, this.zeta8Part, this.imagIntPart, this.zeta8CuPart) match {
    case (0, 0, 0, 0) => 0
    case (_, 0, 0, 0) => 1
    case (_, 0, _, 0) => 2
    case (_, _, 0, _) if Math.abs(this.zeta8Part) == Math.abs(this.zeta8CuPart) => 2
    case _ => 4
  }

  override def trace: Long = 0L

  override def norm: Long = {
    var Na = this.realIntPart * this.realIntPart - this.imagIntPart * this.imagIntPart + 2 * this.zeta8Part * this.zeta8CuPart
    Na *= Na
    var Nb = this.zeta8Part * this.zeta8Part - this.zeta8CuPart * this.zeta8CuPart + 2 * this.realIntPart * this.imagIntPart
    Nb *= Nb
    Na + Nb
  }

  override def minPolynomialCoeffs: Array[Long] = Array(0L, 0L)

  override def minPolynomialString: String = "Not implemented yet"

  private def complexAdjustedNorm: Long = {
    val Na = this.realIntPart + this.zeta8Part - this.zeta8CuPart
    val Nb = this.zeta8Part + this.imagIntPart + this.zeta8CuPart
    Na + Nb
  }

  def complexConjugate: Zeta8Int = new Zeta8Int(this.realIntPart, -this.zeta8CuPart, -this.imagIntPart, -this.zeta8Part)

  override def getRing: IntRing = Zeta8Ring

  override def toString: String = {
    var intermediate: String = this.realIntPart.toString
    intermediate = intermediate + (this.zeta8Part match {
      case -1 => " - zeta_8"
      case 0 => ""
      case 1 => " + zeta_8"
      case _ => " + " + this.zeta8Part.toString + "zeta_8"
    })
    intermediate = intermediate + (this.imagIntPart match {
      case -1 => " - i"
      case 0 => ""
      case 1 => " + i"
      case _ => " + " + this.imagIntPart.toString + "i"
    })
    intermediate = intermediate + (this.zeta8CuPart match {
      case -1 => " - (zeta_8)^3"
      case 0 => ""
      case 1 => " + (zeta_8)^3"
      case _ => " + " + this.zeta8CuPart.toString + "(zeta_8)^3"
    })
    intermediate = intermediate.replace(" + -", " - ")
    if (intermediate.startsWith("0 ")) {
      intermediate = intermediate.substring(2)
    }
    if (intermediate.startsWith("+ ")) {
      intermediate = intermediate.substring(2)
    }
    if (intermediate.startsWith("- ")) {
      intermediate = "-" + intermediate.substring(2)
    }
    intermediate
  }

  override def toUnicodeString: String = "Not implemented yet"

  override def toTeXString: String = "Not implemented yet"

  override def toHTMLString: String = "Not implemented yet"

  override def equals(obj: Any): Boolean = obj match {
    case obj: Zeta8Int => (this.realIntPart == obj.realIntPart
      && this.zeta8Part == obj.zeta8Part
      && this.imagIntPart == obj.imagIntPart
      && this.zeta8CuPart == obj.zeta8CuPart)
    case _ => false
  }

  override def hashCode(): Int = 0

  override def abs: Double = 0.0

  override def angle: Double = 0.0

  override def getRealPartNumeric: Double = this.numValRe

  override def getImagPartNumeric: Double = this.numValIm

  override def convertToQuadInt: QuadInt = (this.realIntPart, this.zeta8Part, this.imagIntPart, this.zeta8CuPart) match {
    case (_, 0, _, 0) => val ringGaussian = new ImagQuadRing(-1)
      new ImagQuadInt(this.realIntPart, this.imagIntPart, ringGaussian)
    case (_, _, 0, _) => if (this.zeta8Part == this.zeta8CuPart) {
      val ringZi2 = new ImagQuadRing(-2)
      new ImagQuadInt(this.realIntPart, this.zeta8Part, ringZi2)
    } else {
      if (this.zeta8Part == -this.zeta8CuPart) {
        val ringZ2 = new RealQuadRing(2)
        new RealQuadInt(this.realIntPart, this.zeta8Part, ringZ2)
      } else {
        val excMsg = "Since " + this.toString + " is of algebraic degree 4, it can't be converted to QuadInt, which can only represent numbers up to degree 2"
        throw new AlgebraicDegreeOverflowException(excMsg, this, this)
      }
    }
    case _ => throw new Exception
  }

  def +(summand: Zeta8Int): Zeta8Int = new Zeta8Int(this.realIntPart + summand.realIntPart,
    this.zeta8Part + summand.zeta8Part,
    this.imagIntPart + summand.imagIntPart,
    this.zeta8CuPart + summand.zeta8CuPart)

  def -(subtrahend: Zeta8Int): Zeta8Int = this + (-subtrahend)

  def unary_- = new Zeta8Int(-this.realIntPart, -this.zeta8Part, -this.imagIntPart, -this.zeta8CuPart)

  def *(multiplicand: Zeta8Int): Zeta8Int = {
    var multRe = this.realIntPart * multiplicand.realIntPart
    var multZ8 = this.realIntPart * multiplicand.zeta8Part
    var multIm = this.realIntPart * multiplicand.imagIntPart
    var multZ8Cu = this.realIntPart * multiplicand.zeta8CuPart
    multZ8 += (this.zeta8Part * multiplicand.realIntPart)
    multIm += (this.zeta8Part * multiplicand.zeta8Part)
    multZ8Cu += (this.zeta8Part * multiplicand.imagIntPart)
    multRe -= (this.zeta8Part * multiplicand.zeta8CuPart)
    multIm += (this.imagIntPart * multiplicand.realIntPart)
    multZ8Cu += (this.imagIntPart * multiplicand.zeta8Part)
    multRe -= (this.imagIntPart * multiplicand.imagIntPart)
    multZ8 -= (this.imagIntPart * multiplicand.zeta8CuPart)
    multZ8Cu += (this.zeta8CuPart * multiplicand.realIntPart)
    multRe -= (this.zeta8CuPart * multiplicand.zeta8Part)
    multZ8 -= (this.zeta8CuPart * multiplicand.imagIntPart)
    multIm -= (this.zeta8CuPart * multiplicand.zeta8CuPart)
    new Zeta8Int(multRe, multZ8, multIm, multZ8Cu)
  }

  @throws(classOf[NotDivisibleException])
  def /(divisor: Zeta8Int): Zeta8Int = {
    val precursor = this * divisor.complexConjugate
    val divisorNorm = divisor.complexAdjustedNorm
    val divRe = new Fraction(precursor.realIntPart, divisorNorm)
    val divZ8 = new Fraction(precursor.zeta8Part, divisorNorm)
    val divIm = new Fraction(precursor.imagIntPart, divisorNorm)
    val divZ8Cu = new Fraction(precursor.zeta8CuPart, divisorNorm)
    (divRe.denominator, divZ8.denominator, divIm.denominator, divZ8Cu.denominator) match {
      case (1, 1, 1, 1) => new Zeta8Int(divRe.numerator.toInt, divZ8.numerator.toInt, divIm.numerator.toInt, divZ8Cu.numerator.toInt)
      case _ => val excMsg = "The number " + divRe.toString + " + " + divZ8.toString + " * zeta_8 + " + divIm.toString + " * i + " + divZ8Cu.toString + " * (zeta_8)^3 is not an algebraic integer"
        val fractions = Array(divRe, divZ8, divIm, divZ8Cu)
        throw new NotDivisibleException(excMsg, this, divisor, fractions)
    }
  }

}
