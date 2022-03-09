package algebraics

trait Arithmeticable[T <: AlgInt] {

  def +(addend: T): T

  def +(addend: Int): T

  def unary_- : T = this * (-1)

  def -(subtrahend: T): T

  def -(subtrahend: Int): T = this + (-subtrahend)

  def *(multiplicand: T): T

  def *(multiplicand: Int): T

  @throws(classOf[NotDivisibleException])
  def /(divisor: T): T

  @throws(classOf[NotDivisibleException])
  def /(divisor: Int): T

}
