package algebraics.quadratics

import algebraics.{IntRing, UnsupportedNumberDomainException}
import algebraics.unary.Z
import calculators.NumberTheoreticFunctionsCalculator

object QuadRing {

  val MAX_ALGEBRAIC_DEGREE: Int = 2

  def apply(d: Int): QuadRing = d match {
    case 1 => val excMsg = "Parameter d = 1 does not generate quadratic extension"
      throw new UnsupportedNumberDomainException(excMsg, Z)
    case _ if d < 0 => new ImagQuadRing(d)
    case _ if d > 1 => new RealQuadRing(d)
  }

}

abstract class QuadRing(d: Int) extends IntRing {
  if (!NumberTheoreticFunctionsCalculator.isSquarefree(d)) {
    val excMsg = "The number " + d.toString + " is not squarefree"
    throw new IllegalArgumentException(excMsg)
  }
  val radicand: Int = d
  private val d1mod4: Boolean = radicand % 4 match {
    case 1 => true
    case -3 => true
    case _ => false
  }

  override def getMaxAlgebraicDegree: Int = QuadRing.MAX_ALGEBRAIC_DEGREE

  override def discriminant: Int = this.radicand * (if (this.d1mod4) 1 else 4)

  override def equals(obj: Any): Boolean = obj match {
    case obj: QuadRing => this.radicand == obj.radicand
    case _ => false
  }

  override def hashCode: Int = if (this.d1mod4) {
    this.radicand - 1
  } else {
    -this.radicand
  }

  override def toString: String = radicand match {
    case -1 => "Z[i]"
    case -3 => "Z[omega]"
    case 5 => "Z[phi]"
    case _ => if (this.d1mod4) {
      "O_(Q(sqrt(" + radicand.toString + ")))"
    } else {
      "Z[sqrt(" + radicand.toString + ")]"
    }
  }

  override def toUnicodeString: String = (radicand match {
    case -1 => IntRing.unicodeZChar + "[i]"
    case -3 => IntRing.unicodeZChar + "[\u03C9]"
    case 5 => IntRing.unicodeZChar + "[\u03C6]"
    case _ => if (this.d1mod4) {
      "O_(" + IntRing.unicodeQChar + "(\u221A(" + radicand.toString + ")))"
    } else {
      IntRing.unicodeZChar + "[\u221A(" + radicand.toString + ")]"
    }
  }).replace("-", "\u2212")

  override def toTeXString: String = radicand match {
    case -1 => IntRing.texZChar + "[i]"
    case -3 => IntRing.texZChar + "[\\omega]"
    case 5 => IntRing.texZChar + "[\\phi]"
    case _ => if (this.d1mod4) {
      "\\mathcal O_{" + IntRing.texQChar + "(\\sqrt{" + radicand.toString + "})}"
    } else {
      IntRing.texZChar + "[\\sqrt{" + radicand.toString + "}]"
    }
  }

  override def toHTMLString: String = (radicand match {
    case -1 => IntRing.htmlZChar + "[<i>i</i>]"
    case -3 => IntRing.htmlZChar + "[&omega;]"
    case 5 => IntRing.htmlZChar + "[&phi;]"
    case _ => if (this.d1mod4) {
      "<i>O</i><sub>" + IntRing.htmlQChar + "(&radic;(" + radicand.toString + "))</sub>"
    } else {
      IntRing.htmlZChar + "[&radic;(" + radicand.toString + ")]"
    }
  }).replace("-", "&minus;")

  override def toFilenameString: String = (radicand match {
    case -1 => "ZI"
    case -3 => "ZW"
    case 5 => "ZPHI"
    case _ => if (this.d1mod4) {
      "OQ" + radicand.toString
    } else {
      "Z" + radicand.toString
    }
  }).replace("-", "I")

  def hasHalfIntegers: Boolean = d1mod4

  def getRadSqrt: Double

  def getAbsNegRad: Int

  def getAbsNegRadSqrt: Double

}
