package algebraics.quadratics

class RealQuadRing(d: Int) extends QuadRing(d: Int) {
  if (d < 2) {
    val excMsg = "The number " + d.toString + " is not a positive integer greater than 1"
    throw new IllegalArgumentException(excMsg)
  }

  override def isPurelyReal: Boolean = true

  override def getRadSqrt: Double = Math.sqrt(d)

  override def getAbsNegRad: Int = radicand

  override def getAbsNegRadSqrt: Double = Math.sqrt(radicand)

}
