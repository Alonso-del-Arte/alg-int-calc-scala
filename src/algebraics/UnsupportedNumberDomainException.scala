package algebraics

@SerialVersionUID(1058737428L)
class UnsupportedNumberDomainException(message: String, ring: IntRing)
  extends RuntimeException(message: String) {
  val unsupportedRing: IntRing = ring
}
