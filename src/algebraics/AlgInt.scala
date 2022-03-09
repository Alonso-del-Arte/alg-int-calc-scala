package algebraics

trait AlgInt {

  def algebraicDegree: Int

  def trace: Long

  def norm: Long

  def minPolynomialCoeffs: Array[Long]

  def minPolynomialString: String

  def getRing: IntRing

  def toUnicodeString: String

  def toTeXString: String

  def toHTMLString: String

  def abs: Double

  def angle: Double

  def getRealPartNumeric: Double

  def getImagPartNumeric: Double

}
