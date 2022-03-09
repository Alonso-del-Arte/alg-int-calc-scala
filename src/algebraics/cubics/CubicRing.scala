package algebraics.cubics

import algebraics.IntRing

object CubicRing {

  val MAX_ALGEBRAIC_DEGREE: Int = 3

}

abstract class CubicRing extends IntRing {

  override def getMaxAlgebraicDegree: Int = CubicRing.MAX_ALGEBRAIC_DEGREE

}
