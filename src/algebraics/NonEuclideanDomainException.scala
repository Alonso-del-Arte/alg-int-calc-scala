package algebraics

@SerialVersionUID(1058723840L)
class NonEuclideanDomainException(message: String, a: AlgInt, b: AlgInt,
                                  eucFn: AlgInt => Long)
  extends Exception(message) {
  val causingA: AlgInt = a
  val causingB: AlgInt = b
  val causingFn: AlgInt => Long = eucFn

}