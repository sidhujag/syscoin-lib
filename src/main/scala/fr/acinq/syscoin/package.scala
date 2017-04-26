package fr.acinq

import java.io._
import java.math.BigInteger

import org.bouncycastle.util.encoders.Hex

/**
  * see https://en.syscoin.it/wiki/Protocol_specification
  */
package object syscoin {
  val Coin = 100000000L
  val Cent = 1000000L
  val MaxMoney = 21000000 * Coin
  val MaxScriptElementSize = 520
  val MaxBlockSize = 1000000
  val LockTimeThreshold = 500000000L

  /**
    * signature hash flags
    */
  val SIGHASH_ALL = 1
  val SIGHASH_NONE = 2
  val SIGHASH_SINGLE = 3
  val SIGHASH_ANYONECANPAY = 0x80

  object Hash {
    val Zeroes: BinaryData = "0000000000000000000000000000000000000000000000000000000000000000"
    val One: BinaryData = "0100000000000000000000000000000000000000000000000000000000000000"
  }

  object SigVersion {
    val SIGVERSION_BASE = 0
    val SIGVERSION_WITNESS_V0 = 1
  }

  sealed trait SysAmount

  case class Satoshi(amount: Long) extends SysAmount {
    // @formatter:off
    def toLong = amount
    def +(other: Satoshi) = Satoshi(amount + other.amount)
    def -(other: Satoshi) = Satoshi(amount - other.amount)
    def *(m: Long) = Satoshi(amount * m)
    def /(d: Long) = Satoshi(amount / d)
    def compare(other: Satoshi): Int = if (amount == other.toLong) 0 else if (amount < other.amount) -1 else 1
    def <= (that: Satoshi): Boolean = compare(that) <= 0
    def >= (that: Satoshi): Boolean = compare(that) >= 0
    def <  (that: Satoshi): Boolean = compare(that) <  0
    def >  (that: Satoshi): Boolean = compare(that) > 0
    // @formatter:on
  }

  implicit object NumericSatoshi extends Numeric[Satoshi] {
    // @formatter:off
    override def plus(x: Satoshi, y: Satoshi): Satoshi = x + y
    override def toDouble(x: Satoshi): Double = x.toLong
    override def toFloat(x: Satoshi): Float = x.toLong
    override def toInt(x: Satoshi): Int = x.toLong.toInt
    override def negate(x: Satoshi): Satoshi = Satoshi(-x.amount)
    override def fromInt(x: Int): Satoshi = Satoshi(x)
    override def toLong(x: Satoshi): Long = x.toLong
    override def times(x: Satoshi, y: Satoshi): Satoshi = ???
    override def minus(x: Satoshi, y: Satoshi): Satoshi = ???
    override def compare(x: Satoshi, y: Satoshi): Int = x.compare(y)
    // @formatter:on
  }

  case class MilliSys(amount: BigDecimal) extends SysAmount

  case class Sys(amount: BigDecimal) extends SysAmount {
    require(amount.abs <= 21e6, "amount must not be greater than 21 millions")
  }

  case class MilliSatoshi(amount: Long) extends SysAmount

  implicit final class SatoshiLong(private val n: Long) extends AnyVal {
    def satoshi = Satoshi(n)
  }

  implicit final class MilliSatoshiLong(private val n: Long) extends AnyVal {
    def millisatoshi = MilliSatoshi(n)
  }

  implicit final class SysDouble(private val n: Double) extends AnyVal {
    def sys = Sys(n)
  }

  implicit final class MilliSysDouble(private val n: Double) extends AnyVal {
    def millisys = MilliSys(n)
  }

  implicit def satoshi2sys(input: Satoshi): Sys = Sys(BigDecimal(input.amount) / Coin)

  implicit def sys2satoshi(input: Sys): Satoshi = Satoshi((input.amount * Coin).toLong)

  implicit def satoshi2millisys(input: Satoshi): MilliSys = sys2millisys(satoshi2sys(input))

  implicit def millisys2satoshi(input: MilliSys): Satoshi = sys2satoshi(millisys2sys(input))

  implicit def sys2millisys(input: Sys): MilliSys = MilliSys(input.amount * 1000L)

  implicit def millisys2sys(input: MilliSys): Sys = Sys(input.amount / 1000L)

  implicit def satoshi2millisatoshi(input: Satoshi): MilliSatoshi = MilliSatoshi(input.amount * 1000L)

  implicit def millisatoshi2satoshi(input: MilliSatoshi): Satoshi = Satoshi(input.amount / 1000L)

  implicit def sys2millisatoshi(input: Sys): MilliSatoshi = satoshi2millisatoshi(sys2satoshi(input))

  implicit def millisatoshi2sys(input: MilliSatoshi): Sys = satoshi2sys(millisatoshi2satoshi(input))

  implicit def millisys2millisatoshi(input: MilliSys): MilliSatoshi = satoshi2millisatoshi(millisys2satoshi(input))

  implicit def millisatoshi2millisys(input: MilliSatoshi): MilliSys = satoshi2millisys(millisatoshi2satoshi(input))

  def toHexString(blob: BinaryData) = Hex.toHexString(blob)

  def fromHexString(hex: String): BinaryData = Hex.decode(hex.stripPrefix("0x"))

  implicit def string2binaryData(input: String): BinaryData = BinaryData(fromHexString(input))

  implicit def seq2binaryData(input: Seq[Byte]): BinaryData = BinaryData(input)

  implicit def array2binaryData(input: Array[Byte]): BinaryData = BinaryData(input)

  implicit def binaryData2array(input: BinaryData): Array[Byte] = input.data.toArray

  implicit def binaryData2Seq(input: BinaryData): Seq[Byte] = input.data

  /**
    *
    * @param input compact size encoded integer as used to encode proof-of-work difficulty target
    * @return a (result, isNegative, overflow) tuple were result is the decoded integer
    */
  def decodeCompact(input: Long): (BigInteger, Boolean, Boolean) = {
    val nSize = (input >> 24).toInt
    val (nWord, result) = if (nSize <= 3) {
      val nWord1 = (input & 0x007fffffL) >> 8 * (3 - nSize)
      (nWord1, BigInteger.valueOf(nWord1))
    } else {
      val nWord1 = (input & 0x007fffffL)
      (nWord1, BigInteger.valueOf(nWord1).shiftLeft(8 * (nSize - 3)))
    }
    val isNegative = nWord != 0 && (input & 0x00800000) != 0
    val overflow = nWord != 0 && ((nSize > 34) || (nWord > 0xff && nSize > 33) || (nWord > 0xffff && nSize > 32))
    (result, isNegative, overflow)
  }

  def isAnyoneCanPay(sighashType: Int): Boolean = (sighashType & SIGHASH_ANYONECANPAY) != 0

  def isHashSingle(sighashType: Int): Boolean = (sighashType & 0x1f) == SIGHASH_SINGLE

  def isHashNone(sighashType: Int): Boolean = (sighashType & 0x1f) == SIGHASH_NONE
}
