package algebraics

object IntRing {

  var preferenceForBlackboardBold: Boolean = true

  def unicodeQChar: String = if (preferenceForBlackboardBold) {
    "\u211A"
  } else {
    "Q"
  }

  def unicodeZChar: String = if (preferenceForBlackboardBold) {
    "\u2124"
  } else {
    "Z"
  }

  def texQChar: String = if (preferenceForBlackboardBold) {
    "\\mathbb Q"
  } else {
    "\\textbf Q"
  }

  def texZChar: String = if (preferenceForBlackboardBold) {
    "\\mathbb Z"
  } else {
    "\\textbf Z"
  }

  def htmlQChar: String = if (preferenceForBlackboardBold) {
    "&#x211A;"
  } else {
    "<b>Q</b>"
  }

  def htmlZChar: String = if (preferenceForBlackboardBold) {
    "&#x2124;"
  } else {
    "<b>Z</b>"
  }

}

trait IntRing {

  def getMaxAlgebraicDegree: Int

  def isPurelyReal: Boolean

  def discriminant: Int

  def toUnicodeString: String

  def toTeXString: String

  def toHTMLString: String

  def toFilenameString: String

}
