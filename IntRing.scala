package algebraics

trait IntRing {

  def getMaxAlgebraicDegree: Int

  def toTeXString: String

  def toHTMLString: String

  def toFilenameString: String

}
