package algebraics.unary

import algebraics.IntRing

object Z extends IntRing {

  var preferBlackboardBold: Boolean = true

  override def getMaxAlgebraicDegree: Int = 1

  override def isPurelyReal: Boolean = true

  override def discriminant: Int = 1

  override def toString: String = "Z"

  override def toUnicodeString: String = "\u2124"

  override def toTeXString: String = if (preferBlackboardBold) {
    "\\mathbb Z"
  } else {
    "\\textbf Z"
  }

  override def toHTMLString: String = if (preferBlackboardBold) {
    "\\u2124"
  } else {
    "<b>Z</b>"
  }

  override def toFilenameString: String = "Z"
}
