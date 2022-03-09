package calculators

import algebraics.{AlgInt, NonEuclideanDomainException, NotDivisibleException}
import algebraics.quadratics.QuadInt

import scala.collection.mutable.ListBuffer

private object EuclideanGCDProcSeq {

  def euclStep(a: QuadInt, b: QuadInt, fn: AlgInt => Long): QuadInt = {
    if (fn(a) < 0 || fn(b) < 0) {
      val excMsg = "The provided function is not valid because it sometimes gives negative values: f(" + a.toString + ") = " + fn(a).toString + ", f(" + b.toString + ") = " + fn(b).toString
      throw new IllegalArgumentException(excMsg)
    }
    try {
      a / b
      QuadInt(0, 0, a.ring)
    } catch {
      case nde: NotDivisibleException => val roundDiv = nde.roundTowardsZero.asInstanceOf[QuadInt]
        val tempMultiple = roundDiv * b
        val remainder = a - tempMultiple
        if (fn(remainder) >= fn(b)) {
          val excMsg = "Domain " + a.getRing.toString + " is not Euclidean for the provided function since f(" + b.toString + ") = " + fn(b).toString + " but f(" + remainder.toString + ") = " + fn(remainder).toString
          throw new NonEuclideanDomainException(excMsg, b, remainder, fn)
        }
        remainder
    }
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
