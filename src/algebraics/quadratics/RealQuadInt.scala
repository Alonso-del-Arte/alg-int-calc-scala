package algebraics.quadratics

class RealQuadInt(a: Int, b: Int, r: RealQuadRing, denom: Int = 1)
  extends QuadInt(a, b, r, denom)
    with Ordered[RealQuadInt] {

  override def abs: Double = Math.abs((this.regPart + this.surdPart * Math.sqrt(this.r.radicand))/this.denominator)

  override def angle: Double = if (this.getRealPartNumeric < 0) {
    Math.PI
  } else {
    0.0
  }

  override def getRealPartNumeric: Double = (this.regPart + Math.sqrt(this.ring.radicand) * this.surdPart)/this.denominator

  override def getImagPartNumeric: Double = 0.0

  override def compare(that: RealQuadInt): Int = if (this == that) {
    0
  } else {
    if (this.ring == that.ring) {
      val diff = this - that
      if (diff.getRealPartNumeric < 0) -1 else 1
    } else {
      val diff = this.getRealPartNumeric - that.getRealPartNumeric
      if (diff < 0) -1 else 1
    }
  }

}
