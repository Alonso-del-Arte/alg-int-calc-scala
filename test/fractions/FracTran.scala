package fractions

import org.junit.Test
import org.junit.Assert._

object FracTran {

  val PRIME_GAME_FRACTS: Array[Fraction] = Array(Fraction(17, 91),
    Fraction(78, 85), Fraction(19, 51), Fraction(23, 38), Fraction(29, 33),
    Fraction(77, 29), Fraction(1, 17), Fraction(11, 13), Fraction(13, 11),
    Fraction(15, 2), Fraction(1, 7), new Fraction(55))

  def iter(fracts: Array[Fraction], n: Long): Long = {
    var fracProd: Fraction = null
    var i: Int = -1
    do {
      i += 1
      fracProd = fracts(i) * n
    } while (fracProd.denominator != 1L)
    fracProd.numerator
  }

  val PRIME_GAME: LazyList[Long] = 2L #:: PRIME_GAME.map(iter(PRIME_GAME_FRACTS, _))

  def process(n: Long, fracts: Array[Fraction]): LazyList[Long] =
    LazyList.iterate(n)(iter(fracts, _))

}

class FracTran {

  @Test def testIterTwo(): Unit = {
    val expected = 15L
    val actual = FracTran.iter(FracTran.PRIME_GAME_FRACTS, 2L)
    assertEquals(expected, actual)
  }

  @Test def testIterEight(): Unit = {
    val expected = 60L
    val actual = FracTran.iter(FracTran.PRIME_GAME_FRACTS, 8L)
    assertEquals(expected, actual)
  }

  @Test def testIter185625(): Unit = {
    val expected = 163125L
    val actual = FracTran.iter(FracTran.PRIME_GAME_FRACTS, 185625L)
    assertEquals(expected, actual)
  }

  @Test def testPrimeGame2To4(): Unit = {
    val expected = List(2, 15, 825, 725, 1925, 2275, 425, 390, 330, 290, 770,
      910, 170, 156, 132, 116, 308, 364, 68, 4)
    val actual = FracTran.PRIME_GAME.takeWhile(_ != 4).toList
    assertEquals(expected, actual)
  }

  @Test def testIteratePrimeGame2To4(): Unit = {
    val expected = 4L
    var actual = 2L
    do {
      actual = FracTran.iter(FracTran.PRIME_GAME_FRACTS, actual)
    } while (actual != java.lang.Long.highestOneBit(actual))
    assertEquals(expected, actual)
  }

}
