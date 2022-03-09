package algebraics.unary

import algebraics.{AlgInt, IntRing}

class UnaryInt(a: Int) extends AlgInt {
  val n: Int = a

  override def algebraicDegree: Int = n match {
    case 0 => 0
    case _ => 1
  }

  override def trace: Long = a

  override def norm: Long = a

  override def minPolynomialCoeffs: Array[Long] = Array(-a, 1)

  override def minPolynomialString: String = a match {
    case 0 => "x"
    case num if num < 0 => "x + " + (-a)
    case _ => "x - " + a
  }

  override def getRing: IntRing = Z

  override def toString: String = a.toString

  override def toUnicodeString: String = a.toString

  override def toTeXString: String = a.toString

  override def toHTMLString: String = a.toString.replace("-", "&minus;")

  override def abs: Double = Math.abs(a)

  override def angle: Double = if (a < 0) {
    Math.PI
  } else {
    0.0
  }

  override def getRealPartNumeric: Double = a.toDouble

  override def getImagPartNumeric: Double = 0.0

}
