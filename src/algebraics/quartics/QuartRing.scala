package algebraics.quartics

import algebraics.IntRing

object QuartRing {

  val MAX_ALGEBRAIC_DEGREE: Int = 4

}

abstract class QuartRing extends IntRing {

  override def getMaxAlgebraicDegree: Int = QuartRing.MAX_ALGEBRAIC_DEGREE

}
