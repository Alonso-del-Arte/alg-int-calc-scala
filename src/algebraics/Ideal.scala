package algebraics

class Ideal(generatorA: AlgInt, generatorB: AlgInt) {
//  private val principalIdealFlag: Boolean = false
//  private val wholeRingFlag: Boolean = false

  def this(generatorA: AlgInt) {
    this(generatorA, null)
  }

  def norm: Long = 0L

  def isPrincipal: Boolean = false

  def isMaximal: Boolean = false

  def contains(number: AlgInt): Boolean = false

  def contains(ideal: Ideal): Boolean = false

  def toUnicodeString: String = "Not implemented yet"

  def toTeXString: String = "Not implemented yet"

  def toHTMLString: String = "Not implemented yet"

}
