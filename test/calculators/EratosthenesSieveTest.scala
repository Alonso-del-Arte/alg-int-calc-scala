package calculators

import org.junit.Test
import org.junit.Assert._

class EratosthenesSieveTest {

  val primeSieve = new EratosthenesSieve(10000)

  @Test def testApply(): Unit = {
    var assertionMessage = "Fourth prime should be 7"
    var expected = 7
    var actual = primeSieve(3)
    assertEquals(assertionMessage, expected, actual)
    assertionMessage = "25th prime should be 97"
    expected = 97
    actual = primeSieve(24)
    assertEquals(assertionMessage, expected, actual)
    expected = 997
    actual = primeSieve(167)
    assertEquals(assertionMessage, expected, actual)
    expected = 9973
    actual = primeSieve(1228)
    assertEquals(assertionMessage, expected, actual)
  }

  @Test(expected = classOf[IndexOutOfBoundsException])
  def testNegativeIndex(): Unit = {
    val result = primeSieve(-10)
    println("Sieve reports prime(-10) = " + result)
  }

  @Test(expected = classOf[IndexOutOfBoundsException])
  def testExcessiveIndex(): Unit = {
    val result = primeSieve(2000)
    println("Sieve reports prime(2000) = " + result)
  }

}
