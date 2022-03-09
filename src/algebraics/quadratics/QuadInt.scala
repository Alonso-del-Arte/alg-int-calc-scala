package algebraics.quadratics

import algebraics.{AlgebraicDegreeOverflowException, AlgInt, Arithmeticable, IntRing, NotDivisibleException, UnsupportedNumberDomainException}
import fractions.Fraction

object QuadInt {

  def apply(a: Int, b: Int, r: QuadRing, denom: Int = 1): QuadInt = r match {
    case iR: ImagQuadRing => new ImagQuadInt(a, b, iR, denom)
    case rR: RealQuadRing => new RealQuadInt(a, b, rR, denom)
    case _ => val excMsg = "The ring " + r.toString + " of type " + r.getClass.getName + " is not currently supported for QuadInt.appply()"
      throw new UnsupportedNumberDomainException(excMsg, r)
  }

}

abstract class QuadInt(a: Int, b: Int, r: QuadRing, denom: Int = 1)
  extends AlgInt with Arithmeticable[QuadInt] {
  if (denom == 0) {
    throw new IllegalArgumentException("0 is invalid for denom parameter")
  }
  private var adjuster = if (denom < 0) -1 else 1
  val denominator: Int = (a % 2, b % 2, denom * adjuster) match {
    case (0, 0, 2) => adjuster *= 2
      1
    case _ => denom * adjuster
  }
  if (this.denominator > 2) {
    val excMsg = denom.toString + " is invalid for denom parameter"
    throw new IllegalArgumentException(excMsg)
  }
  if (this.denominator == 2) {
    if (r.hasHalfIntegers) {
      if (Math.abs(a) % 2 == Math.abs(b) % 2) {
        if (a % 2 == 0) {
          adjuster *= 2
        }
      } else {
        val excMsg = "Parity of parameters a = " + a.toString + " and b = " + b.toString + " mismatch"
        throw new IllegalArgumentException(excMsg)
      }
    } else {
      val excMsg = "The ring " + r.toString + " does not have \"half-integers\""
      throw new IllegalArgumentException(excMsg)
    }
  }
  val regPart: Int = a / adjuster
  val surdPart: Int = b / adjuster
  val ring: QuadRing = r

  override def algebraicDegree: Int = (this.regPart, this.surdPart) match {
    case (0, 0) => 0
    case (_, 0) => 1
    case _ => 2
  }

  override def trace: Long = this.denominator match {
    case 1 => 2 * this.regPart
    case 2 => this.regPart
  }

  override def norm: Long = (this.regPart.toLong * this.regPart.toLong - this.ring.radicand * this.surdPart.toLong * this.surdPart.toLong)/(this.denominator * this.denominator)

  override def minPolynomialCoeffs: Array[Long] = if (this.algebraicDegree == 2) {
    Array(this.norm, -this.trace, 1L)
  } else {
    Array(-this.regPart, 1L, 0L)
  }

  override def minPolynomialString: String = (this.algebraicDegree match {
    case 0 => "x"
    case 1 => "x + " + (-this.regPart).toString
    case 2 => if (this.regPart == 0) {
      "x^2 + " + this.norm.toString
    } else {
      "x^2 + " + (-this.trace).toString + " + " + this.norm.toString
    }
  }).replace("+ -", "- ")

  def conjugate: QuadInt = QuadInt(this.regPart, -this.surdPart, this.ring, this.denominator)

  override def getRing: IntRing = ring

  override def toString: String = {
    if (this.denominator == 1) {
      var quadIntString = (this.regPart, this.surdPart) match {
        case (_, 0) => this.regPart.toString
        case (0, -1) => "-sqrt(" + this.ring.radicand.toString + ")"
        case (0, 1) => "sqrt(" + this.ring.radicand.toString + ")"
        case (0, _) => this.surdPart.toString + "sqrt(" + this.ring.radicand.toString + ")"
        case (_, -1) => this.regPart.toString + " - sqrt(" + this.ring.radicand.toString + ")"
        case (_, 1) => this.regPart.toString + " + sqrt(" + this.ring.radicand.toString + ")"
        case (_, _) => this.regPart.toString + " + " + this.surdPart.toString + "sqrt(" + this.ring.radicand.toString + ")"
      }
      quadIntString = quadIntString.replace("+ -", "- ")
      if (this.ring.radicand == -1) {
        quadIntString = quadIntString.replace("sqrt(-1)", "i")
      }
      quadIntString
    } else {
      (if (this.surdPart < 0) {
        this.regPart.toString + "/2 - " + (-this.surdPart).toString + "sqrt(" + this.ring.radicand.toString + ")/2"
      } else {
        this.regPart.toString + "/2 + " + this.surdPart.toString + "sqrt(" + this.ring.radicand.toString + ")/2"
      }).replace(" 1sqrt", " sqrt")
    }
  }

  def toStringAlt: String = if (this.ring.hasHalfIntegers) {
    val thetaLetter = this.ring.radicand match {
      case -3 => "omega"
      case 5 => "phi"
      case _ => "theta"
    }
    val adjuster = if (this.denominator == 1) 2 else 1
    val thetaPart = this.surdPart * adjuster
    val nonThetaPart = this.ring.radicand match {
      case -3 => (this.regPart * adjuster + thetaPart) / 2
      case _ => (this.regPart * adjuster - thetaPart) / 2
    }
    (nonThetaPart, thetaPart) match {
      case (0, 0) => "0"
      case (0, -1) => "-" + thetaLetter
      case (0, 1) => thetaLetter
      case (0, _) => thetaPart.toString + thetaLetter
      case (_, 0) => nonThetaPart.toString
      case (_, -1) => nonThetaPart.toString + " - " + thetaLetter
      case (_, 1) => nonThetaPart.toString + " + " + thetaLetter
      case (_, _) if thetaPart < 0 => nonThetaPart.toString + " - " + Math.abs(thetaPart).toString
      case _ => nonThetaPart.toString + " + " + thetaPart.toString + thetaLetter
    }
  } else {
    this.toString
  }

  override def toUnicodeString: String = {
    var intermediate = this.toString
    intermediate = intermediate.replace("-", "\u2212")
    intermediate.replace("sqrt", "\u221A")
  }

  def toUnicodeStringAlt: String = if (this.ring.hasHalfIntegers) {
    var intermediate = this.toStringAlt
    intermediate = intermediate.replace("omega", "\u03C9")
    intermediate = intermediate.replace("theta", "\u03B8")
    intermediate = intermediate.replace("phi", "\u03C6")
    intermediate.replace("-", "\u2212")
  } else this.toUnicodeString

  override def toTeXString: String = if (this.denominator == 2) {
    var intermediate = "\\frac{" + this.regPart.toString + "}{2} + \\frac{" + this.surdPart.toString + " \\sqrt{" + this.ring.radicand.toString + "}}{2}"
    intermediate = intermediate.replace("{2} + \\frac{-", "{2} - \\frac{")
    intermediate = intermediate.replace("\\frac{-", "-\\frac{")
    intermediate.replace("\\frac{1 \\sqrt", "\\frac{\\sqrt")
  } else {
    var intermediate = this.toString
    intermediate = intermediate.replace("sqrt", "\\sqrt")
    intermediate = intermediate.replace("(", "{")
    intermediate = intermediate.replace(")", "}")
    intermediate
  }

  def toTeXStringAlt: String = if (this.ring.hasHalfIntegers) {
    var intermediate = this.toStringAlt
    intermediate = intermediate.replace("omega", "\\omega")
    intermediate = intermediate.replace("theta", "\\theta")
    intermediate.replace("phi", "\\phi")
  } else this.toTeXString

  override def toHTMLString: String = {
    var intermediate = this.toString
    intermediate = intermediate.replace("i", "<i>i</i>")
    intermediate = intermediate.replace("-", "&minus;")
    intermediate.replace("sqrt", "&radic;")
  }

  def toHTMLStringAlt: String = if (this.ring.hasHalfIntegers) {
    var intermediate = this.toStringAlt
    intermediate = intermediate.replace("omega", "&omega;")
    intermediate = intermediate.replace("theta", "&theta;")
    intermediate = intermediate.replace("phi", "&phi;")
    intermediate.replace("-", "&minus;")
  } else this.toHTMLString

  override def equals(obj: Any): Boolean = obj match {
    case obj: QuadInt => if (this.surdPart == 0 && obj.surdPart == 0) {
      this.regPart == obj.regPart
    } else {
      this.regPart == obj.regPart && this.surdPart == obj.surdPart && this.ring == obj.ring && this.denominator == obj.denominator
    }
    case _ => false
  }

  override def hashCode: Int = if (this.surdPart == 0) {
    this.regPart
  } else {
    this.regPart + this.surdPart * 32 + this.ring.hashCode * 1024 + this.denominator * 536870912
  }

  override def +(addend: QuadInt): QuadInt = if (this.ring == addend.ring) {
    (this.denominator, addend.denominator) match {
      case (1, 2) => QuadInt(2 * this.regPart + addend.regPart, 2 * this.surdPart + addend.surdPart, this.ring, 2)
      case (2, 1) => QuadInt(this.regPart + 2 * addend.regPart, this.surdPart + 2 * addend.surdPart, this.ring, 2)
      case _ => QuadInt(this.regPart + addend.regPart, this.surdPart + addend.surdPart, this.ring, this.denominator)
    }
  } else {
    (this.surdPart, addend.surdPart, this.denominator, addend.denominator) match {
      case (0, 0, 1, 1) => QuadInt(this.regPart + addend.regPart, this.surdPart + addend.surdPart, this.ring, this.denominator)
      case (0, _, 1, 1) => QuadInt(this.regPart + addend.regPart, addend.surdPart, addend.ring)
      case (0, _, 1, 2) => QuadInt(2 * this.regPart + addend.regPart, addend.surdPart, addend.ring, 2)
      case (_, 0, 1, 1) => QuadInt(this.regPart + addend.regPart, this.surdPart, this.ring)
      case (_, 0, 2, 1) => QuadInt(this.regPart + 2 * addend.regPart, this.surdPart, this.ring, 2)
      case _ => val excMsg = "(" + this.toString + ") + (" + addend.toString + ") is a number of degree 4, which QuadInt can't represent"
        throw new AlgebraicDegreeOverflowException(excMsg, this, addend)
    }
  }

  override def +(addend: Int): QuadInt = QuadInt(this.regPart + this.denominator * addend, this.surdPart, this.ring, this.denominator)

  override def unary_- : QuadInt = QuadInt(-this.regPart, -this.surdPart, this.ring, this.denominator)

  override def -(subtrahend: QuadInt): QuadInt = this + (-subtrahend)

  override def -(subtrahend: Int): QuadInt = this + (-subtrahend)

  override def *(multiplicand: QuadInt): QuadInt = if (this.ring == multiplicand.ring) {
    if (this.denominator == 2 || multiplicand.denominator == 2) {
      val regA = new Fraction(this.regPart, this.denominator)
      val surdA = new Fraction(this.surdPart, this.denominator)
      val regB = new Fraction(multiplicand.regPart, multiplicand.denominator)
      val surdB = new Fraction(multiplicand.surdPart, multiplicand.denominator)
      val multReg = (regA * regB) + (surdA * surdB * this.ring.radicand)
      val multSurd = (regA * surdB) + (surdA * regB)
      QuadInt(multReg.numerator.toInt, multSurd.numerator.toInt, this.ring, multReg.denominator.toInt)
    } else {
      val multReg = this.regPart * multiplicand.regPart + this.surdPart * multiplicand.surdPart * this.ring.radicand
      val multSurd = this.regPart * multiplicand.surdPart + this.surdPart * multiplicand.regPart
      QuadInt(multReg, multSurd, this.ring)
    }
  } else {
    (this.regPart, multiplicand.regPart, this.surdPart, multiplicand.surdPart) match {
      case (0, 0, _, _) => val signAdjust = if (this.ring.isPurelyReal || multiplicand.ring.isPurelyReal) {
          1
        } else {
          -1
        }
        val d = this.ring.radicand * multiplicand.ring.radicand
        QuadInt(0, this.surdPart * multiplicand.surdPart * signAdjust, QuadRing(d))
      case (_, _, 0, 0) => QuadInt(this.regPart * multiplicand.regPart, 0, this.ring)
      case (_, _, 0, _) => val multReg = this.regPart * multiplicand.regPart
        val multSurd = this.regPart * multiplicand.surdPart
        QuadInt(multReg, multSurd, multiplicand.ring, multiplicand.denominator)
      case (_, _, _, 0) => val multReg = this.regPart * multiplicand.regPart
        val multSurd = this.surdPart * multiplicand.regPart
        QuadInt(multReg, multSurd, this.ring, this.denominator)
      case _ => val excMsg = "The product of " + this.toString + " by " + multiplicand.toString + " is an algebraic integer of degree 4"
        throw new AlgebraicDegreeOverflowException(excMsg, this, multiplicand)
    }
  }

  override def *(multiplicand: Int): QuadInt = QuadInt(this.regPart * multiplicand, this.surdPart * multiplicand, this.ring, this.denominator)

  @throws(classOf[NotDivisibleException])
  override def /(divisor: QuadInt): QuadInt = if (this.ring == divisor.ring) {
    if (divisor.norm == 0) {
      throw new IllegalArgumentException("Division by zero is not allowed")
    }
    val dividendRegFract = new Fraction(this.regPart, this.denominator)
    val dividendSurdFract = new Fraction(this.surdPart, this.denominator)
    val divisorRegFract = new Fraction(divisor.regPart, divisor.denominator)
    val divisorSurdFract = new Fraction(divisor.surdPart, divisor.denominator)
    val quotientRegFract = (dividendRegFract * divisorRegFract - this.ring.radicand * dividendSurdFract * divisorSurdFract)/divisor.norm
    val quotientSurdFract = (dividendSurdFract * divisorRegFract - dividendRegFract * divisorSurdFract)/divisor.norm
    val notDivisibleFlag = (this.ring.hasHalfIntegers, quotientRegFract.denominator, quotientSurdFract.denominator) match {
      case (_, 1, 1) => false
      case (true, 2, 2) => false
      case _ => true
    }
    if (notDivisibleFlag) {
      val excMsg = this.toString + " divided by " + divisor.toString + " is " + quotientRegFract.toString + " + " + quotientSurdFract.toString + " * sqrt(" + this.ring.radicand.toString + "), which is not an algebraic integer"
      val fractArray = Array(quotientRegFract, quotientSurdFract)
      throw new NotDivisibleException(excMsg, this, divisor, fractArray)
    }
    QuadInt(quotientRegFract.numerator.toInt, quotientSurdFract.numerator.toInt, this.ring, quotientRegFract.denominator.toInt)
  } else (this.regPart, divisor.regPart, this.surdPart, divisor.surdPart) match {
    case (0, 0, _, _) => val quotientSurdFract = new Fraction(this.surdPart, divisor.surdPart)
      if (this.ring.radicand % divisor.ring.radicand == 0) {
        val divRing = QuadRing(this.ring.radicand / divisor.ring.radicand)
        if (quotientSurdFract.denominator == 1) {
          QuadInt(0, quotientSurdFract.numerator.toInt, divRing)
        } else {
          val excMsg = this.toString + " is not divisible by " + divisor.toString
          val fractions = Array(new Fraction(0), quotientSurdFract)
          throw new NotDivisibleException(excMsg, this, divisor, fractions)
        }
      } else {
        val excMsg = this.toString + " is not divisible by " + divisor.toString
        val fractions = Array(new Fraction(0), quotientSurdFract)
        throw new NotDivisibleException(excMsg, this, divisor, fractions)
      }
    case (_, _, 0, _) => QuadInt(this.regPart, 0, divisor.ring) / divisor
    case (_, _, _, 0) => this / divisor.regPart
    case _ => val excMsg = "The number " + this.toString + " divided by " + divisor.toString + " is an algebraic number of degree 4"
      throw new AlgebraicDegreeOverflowException(excMsg, this, divisor)
  }

  @throws(classOf[NotDivisibleException])
  override def /(divisor: Int): QuadInt = {
    val divRegFract = new Fraction(this.regPart, this.denominator * divisor)
    val divSurdFract = new Fraction(this.surdPart, this.denominator * divisor)
    (this.ring.hasHalfIntegers, divRegFract.denominator, divSurdFract.denominator) match {
      case (_, 1, 1) => QuadInt(divRegFract.numerator.toInt, divSurdFract.numerator.toInt, this.ring)
      case (true, 2, 2) => QuadInt(divRegFract.numerator.toInt, divSurdFract.numerator.toInt, this.ring, 2)
      case _ => throw new NotDivisibleException(this, QuadInt(divisor, 0, this.ring), Array(divRegFract, divSurdFract))
    }
  }

}
