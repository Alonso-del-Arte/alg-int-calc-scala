package algebraics.quartics

import algebraics.IntRing

object Zeta8Ring extends QuartRing {

  override def isPurelyReal: Boolean = false

  override def discriminant: Int = 256

  override def toString: String = "O_Q(zeta_8)"

  override def toUnicodeString: String = "O_" + IntRing.unicodeQChar + "(\u03B6\u2088)"

  override def toTeXString: String = "\\mathcal O_{" + IntRing.texQChar +"(\\zeta_8)}"

  override def toHTMLString: String = "<i>O</i><sub>" + IntRing.htmlQChar + "(&zeta;<sub>8</sub>)</sub>"

  override def toFilenameString: String = "O_QZETA_8"

}
