package fr.acinq.syscoin

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SysAmountSpec extends FunSuite {

  test("sys/satoshi conversions") {
    val x = 12.34567 sys
    val y: MilliSys = x
    val z: Satoshi = x
    val z1: Satoshi = y
    assert(z === z1)
    assert(y.amount === BigDecimal(12345.67))
    assert(z.amount === 1234567000L)
    val x1: Sys = z1
    assert(x1 === x)
    val x2: MilliSys = z1
    assert(x2 === y)

    val z3: MilliSatoshi = x

    val z4: MilliSatoshi = y
    assert(z3 == z4)
    assert(z3.amount == 1234567000000L)

    val z5 = 1234567000000L millisatoshi
    val x4: Sys = z5
    assert(x4 == x)
  }

  test("conversions overflow") {
    intercept[IllegalArgumentException] {
      val toomany = 22e6 sys
    }
  }

  test("basic operations") {
    val x = 1.1 sys
    val y: Sys = x - Satoshi(50000)
    val z: Satoshi = y
    assert(z === Satoshi(109950000))
    assert(z + z === Satoshi(109950000 + 109950000))
    assert(z + z - z === z)
    assert((z + z) / 2 === z)
    assert((z * 3) / 3 === z)
    assert(Seq(500 satoshi, 100 satoshi, 50 satoshi).sum === Satoshi(650))
  }

  test("basic comparisons") {
    val x: Satoshi = 1.001 sys
    val y: Satoshi = 1 sys
    val z: Satoshi = 1 millisys

    assert(x >= x)
    assert(x <= x)
    assert(x > y)
    assert(y < x)
    assert(x < y + z + z)
    assert(x == y + z)
  }
}
