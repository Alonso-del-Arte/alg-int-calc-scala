package algebraics

class NonUniqueFactorizationDomainException(message: String, number: AlgInt)
  extends Exception(message: String) {
  val unfactorizedNumber: AlgInt = number

  def tryToFactorizeAnyway: Vector[AlgInt] = Vector[AlgInt](this.unfactorizedNumber, this.unfactorizedNumber)

}
