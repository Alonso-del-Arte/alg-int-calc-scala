package algebraics.quadratics

class IllDefQuadRing(d: Int) extends QuadRing(d) {

  override def isPurelyReal: Boolean = this.radicand > -1

  override def getRadSqrt: Double = if (this.radicand > -1) {
    Math.sqrt(Math.abs(this.radicand))
  } else {
    val excMsg = "The square root of " + radicand.toString + " is a purely imaginary number"
    throw new UnsupportedOperationException(excMsg)
  }

  override def getAbsNegRad: Int = Math.abs(this.radicand)

  override def getAbsNegRadSqrt: Double = Math.sqrt(Math.abs(this.radicand))

}
