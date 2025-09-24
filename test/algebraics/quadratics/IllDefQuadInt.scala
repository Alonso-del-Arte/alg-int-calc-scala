package algebraics.quadratics

/**
  * Represents an ill-defined quadratic integer.
  * @param a The parameter a.
  * @param b The parameter b.
  * @param r The ring.
  * @param denom The denominator. By default 1.
  * @deprecated This class will be removed once the alternative is in place.
  */
@Deprecated
class IllDefQuadInt(a: Int, b: Int, r: IllDefQuadRing, denom: Int = 1)
  extends QuadInt(a, b, r, denom) {

  override def abs: Double = 0.0

  override def angle: Double = 0.0

  override def getRealPartNumeric: Double = (this.regPart.toDouble + (if (this.ring.isPurelyReal) {
    0.0
  } else {
    this.surdPart.toDouble * this.ring.getRadSqrt
  }))/this.denominator

  override def getImagPartNumeric: Double = if (this.ring.isPurelyReal) {
    0.0
  } else {
    this.surdPart.toDouble * this.ring.getAbsNegRadSqrt/this.denominator
  }

}
