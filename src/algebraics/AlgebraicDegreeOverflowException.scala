package algebraics

@SerialVersionUID(1058722816L)
class AlgebraicDegreeOverflowException(message: String, numberA: AlgInt, numberB: AlgInt)
  extends RuntimeException(message: String) {
  val maxExpectedAlgebraicDegree: Int = numberA.algebraicDegree
  val diffRingNumberA: AlgInt = numberA
  val diffRingNumberB: AlgInt = numberB
  val necessaryAlgebraicDegree: Int = this.diffRingNumberA.algebraicDegree * this.diffRingNumberB.algebraicDegree
}
