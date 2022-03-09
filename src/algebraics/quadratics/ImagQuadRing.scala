package algebraics.quadratics

class ImagQuadRing(d: Int) extends QuadRing(d: Int) {
  if (d > -1) {
    val excMsg = "The number " + d.toString + " is not negative"
    throw new IllegalArgumentException(excMsg)
  }

  override def isPurelyReal: Boolean = false

  override def getRadSqrt: Double = {
    val excMsg = "The square root of " + radicand.toString + " is a purely imaginary number"
    throw new UnsupportedOperationException(excMsg)
  }

  override def getAbsNegRad: Int = -radicand

  override def getAbsNegRadSqrt: Double = Math.sqrt(-radicand)

}
