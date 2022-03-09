package fractions

class FractionRange(start: Fraction, end: Fraction, step: Fraction)
  extends IndexedSeq[Fraction] {
  val startFraction: Fraction = start
  val endFraction: Fraction = end
  val stepFraction: Fraction = step
  private[this] val inferredStepFlag: Boolean = this.stepFraction == Fraction.inferStep(this.startFraction, this.endFraction)
  private[this] var incr: Fraction = (this.endFraction - this.startFraction)/this.stepFraction
  if (incr.denominator != 1) {
    val excMsg = "Step " + step.toString + " is invalid for FractionRange from " + start.toString + " to " + end.toString + "."
    throw new IllegalArgumentException(excMsg)
  }
  if (incr.numerator < 0) {
    incr = -incr
  }
  private[this] val len: Int = incr.numerator.toInt + 1

  def this(start: Fraction, end: Fraction) {
    this(start, end, Fraction.inferStep(start, end))
  }

  override def toString: String = {
    var rangeString = this.startFraction.toString + " to " + this.endFraction.toString
    if (!this.inferredStepFlag) {
      rangeString = rangeString + " by " + this.stepFraction.toString
    }
    rangeString
  }

  override def equals(obj: Any): Boolean = obj match {
    case obj: FractionRange => this.startFraction == obj.startFraction && this.endFraction == obj.endFraction && this.stepFraction == obj.stepFraction
    case _ => false
  }

  override def hashCode: Int = (this.endFraction.hashCode - this.startFraction.hashCode) * this.stepFraction.hashCode

  override def length: Int = this.len

  override def apply(idx: Int): Fraction = {
    if (idx < 0) {
      val excMsg = "Negative index " + idx.toString + " is invalid"
      throw new IndexOutOfBoundsException(excMsg)
    }
    if (idx >= this.len) {
      val excMsg = "Index " + idx.toString + " is invalid since range only has " + this.len.toString + " elements."
      throw new IndexOutOfBoundsException(excMsg)
    }
    this.startFraction + (this.stepFraction * idx)
  }

  def by(newStep: Fraction): FractionRange = {
    new FractionRange(start, end, newStep)
  }

}
