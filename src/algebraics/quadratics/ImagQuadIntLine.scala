package algebraics.quadratics

import algebraics.NotDivisibleException

class ImagQuadIntLine(start: ImagQuadInt, end: ImagQuadInt, step: ImagQuadInt)
  extends IndexedSeq[ImagQuadInt]{
  val startNumber: ImagQuadInt = start
  val finishNumber: ImagQuadInt = end
  val stepNumber: ImagQuadInt = step
  private[this] val inferredStepFlag: Boolean = this.stepNumber == ImagQuadInt.inferStep(this.startNumber, this.finishNumber)
  private[this] val incr: QuadInt = try {
    (this.finishNumber - this.startNumber)/this.stepNumber
  } catch {
    case nde: NotDivisibleException => val excMsg = step.toString + " is not a valid step"
      throw new IllegalArgumentException(excMsg, nde)
  }
  private[this] val len: Int = incr.regPart + 1
  private[this] val endCheck = this.startNumber + stepNumber * (len - 1)
  if (endCheck != this.finishNumber) {
    val excMsg = stepNumber.toString + " is not a valid step because " + (this.stepNumber * (len - 1)).toString + " plus " + this.startNumber.toString + " does not match " + this.finishNumber.toString
    throw new IllegalArgumentException(excMsg)
  }

  def this(start: ImagQuadInt, end: ImagQuadInt) {
    this(start, end, ImagQuadInt.inferStep(start, end))
  }

  override def toString: String = {
    this.startNumber.toString + " to " + this.finishNumber.toString + (if (inferredStepFlag) {
      ""
    } else {
      " by " + this.stepNumber.toString
    })
  }

  override def length: Int = len

  override def apply(index: Int): ImagQuadInt = {
    if (index < 0) {
      val excMsg = "Negative index " + index.toString + " is not valid"
      throw new IndexOutOfBoundsException(excMsg)
    }
    if (index >= len) {
      val excMsg = "Index " + index.toString + " is beyond last index, which is " + (len - 1).toString
      throw new IndexOutOfBoundsException(excMsg)
    }
    (this.startNumber + this.stepNumber * index).asInstanceOf[ImagQuadInt]
  }

  def by(newStep: ImagQuadInt): ImagQuadIntLine = new ImagQuadIntLine(this.startNumber, this.finishNumber, newStep)

}
