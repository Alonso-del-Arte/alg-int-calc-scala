package algebraics.quadratics

import algebraics.NotDivisibleException

object ImagQuadInt {

  def inferStep(start: ImagQuadInt, end: ImagQuadInt): ImagQuadInt = {
    var step = end - start
    var keepGoing = true
    var divisor = 2
    while (keepGoing) {
      try {
        step = step / divisor
        keepGoing = step.abs > 1.0
        divisor = 1
      } catch {
        case nde: NotDivisibleException => keepGoing = nde.getAbs > 1.0
      }
      divisor = divisor + 1
    }
    step.asInstanceOf[ImagQuadInt]
  }

}

class ImagQuadInt(a: Int, b: Int, r: ImagQuadRing, denom: Int = 1)
  extends QuadInt(a, b, r, denom) {
  private val numValRe = this.regPart.toDouble / this.denominator
  private val numValIm = this.ring.getAbsNegRadSqrt * this.surdPart / this.denominator

  override def abs: Double = {
    val reSq = this.regPart * this.regPart
    val imSq = -this.ring.radicand * this.surdPart * this.surdPart
    val denomSq = this.denominator * this.denominator
    Math.sqrt((reSq.toDouble + imSq.toDouble)/denomSq.toDouble)
  }

  override def angle: Double = {
    Math.atan2(this.numValIm, this.numValRe)
  }

  override def getRealPartNumeric: Double = this.regPart.toDouble / this.denominator.toDouble

  override def getImagPartNumeric: Double = this.surdPart.toDouble * this.ring.getAbsNegRadSqrt / this.denominator.toDouble

  def to(end: ImagQuadInt): ImagQuadIntLine = {
    new ImagQuadIntLine(this, end, ImagQuadInt.inferStep(this, end))
  }

}
