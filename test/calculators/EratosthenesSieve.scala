package calculators

import scala.collection.mutable.ArrayBuffer

/**
  * A simple implementation of the sieve of Eratosthenes for prime numbers.
  * This class is intended only for the tests, not production.
  * @param threshold The largest number to sieve up to. The constructor may
  *                  sieve slightly beyond this number.
  */
class EratosthenesSieve(threshold: Int) {
  private val list = new ArrayBuffer[Int]()
  list += 2
  list += 3
  list += 5
  list += 7
  list += 11
  list += 13
  list += 17
  list += 19
  list += 23
  list += 29
  private val modsList = Array(1, 7, 11, 13, 17, 19, 23, 29)
  private var currMult = 30
  while (currMult < threshold) {
    val potentialPrimes = modsList.map(_ + currMult)
    for (i <- 0 to 7) {
      val currPotentialPrime = potentialPrimes(i)
      val primesToCheck = list.filter(_ <= Math.sqrt(currPotentialPrime))
      val mods = primesToCheck.map(currPotentialPrime % _)
      if (!mods.contains(0)) {
        list += currPotentialPrime
      }
    }
    currMult += 30
  }

  /**
    * Gives the prime number at a specified index.
    * @param index The index for the prime number to retrieve. The first prime,
    *              2, is at index 0, the second is 3 at index 1, the third prime
    *              5 is at index 1, etc.
    * @return The prime number specified by the index, e.g., 97 for index 24.
    * @throws IndexOutOfBoundsException If the index is negative, or if the
    *                                   index is greater than what was sieved
    *                                   up to at initialization.
    */
  def apply(index: Int): Int = {
    if (index < 0) {
      val excMsg = s"Index $index is out of bounds"
      throw new IndexOutOfBoundsException(excMsg)
    }
    if (index >= list.size) {
      val excMsg = s"Index $index out of bounds, only ${list.size} available"
      throw new IndexOutOfBoundsException(excMsg)
    }
    list(index)
  }

}
