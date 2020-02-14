package calculators

import algebraics.{AlgInt, NonEuclideanDomainException, NotDivisibleException}
import algebraics.quadratics.QuadInt

import scala.collection.mutable.ListBuffer

private object EuclideanGCDProcSeq {

  def euclStep(a: QuadInt, b: QuadInt, fn: AlgInt => Long): QuadInt = {
    QuadInt(0, 0, a.ring)
  }

}

class EuclideanGCDProcSeq(val a: QuadInt, val b: QuadInt,
                          val fn: AlgInt => Long = NumberTheoreticFunctionsCalculator.normWrap) {
  if (!a.getRing.equals(b.getRing)) {
    val excMsg = "GCD of " + a.toString + " and " + b.toString + " is in a ring of higher degree"
    throw new IllegalArgumentException(excMsg)
  }
  private var currA = a
  private var currB = b
  private var seq = ListBuffer(a, b)
  var problem: NonEuclideanDomainException = _
  try {
    while (fn(currB) != 0) {
      val currRem = EuclideanGCDProcSeq.euclStep(currA, currB, fn)
      seq += currRem
      currA = currB
      currB = currRem
    }
  } catch {
    case nede: NonEuclideanDomainException => problem = nede
  }
  val result: QuadInt = if (problem == null) currA else QuadInt(-1, 0, a.ring)

  def euclideanSeq: List[QuadInt] = seq.toList

}
