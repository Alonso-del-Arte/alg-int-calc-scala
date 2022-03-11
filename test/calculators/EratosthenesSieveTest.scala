package calculators

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

class EratosthenesSieveTest {
  private val threshold = 10000
  val primeSieve = new EratosthenesSieve(threshold)

  @Test def testApply(): Unit = {
    var msg = "Fourth prime should be 7"
    var expected = 7
    var actual = primeSieve(3)
    assertEquals(expected, actual, msg)
    msg = "25th prime should be 97"
    expected = 97
    actual = primeSieve(24)
    assertEquals(expected, actual, msg)
    expected = 997
    actual = primeSieve(167)
    assertEquals(expected, actual, msg)
    expected = 9973
    actual = primeSieve(1228)
    assertEquals(expected, actual, msg)
  }

  @Test
  def testNegativeIndex(): Unit = {
    val index = -10
    val exc = assertThrows(classOf[IndexOutOfBoundsException], () => {
      val result = primeSieve(index)
      println(s"Sieve reports prime($index) = $result")
    })
    val excMsg = exc.getMessage
    assert(excMsg != null, "Message should not be null")
    println("\"" + excMsg + "\"")
  }

  @Test
  def testExcessiveIndex(): Unit = {
    val index = 2000
    val exc = assertThrows(classOf[IndexOutOfBoundsException], () => {
      val result = primeSieve(index)
      println(s"Sieve reports prime($index) = $result")
    })
    val excMsg = exc.getMessage
    assert(excMsg != null, "Message should not be null")
    println("\"" + excMsg + "\"")
  }

}
