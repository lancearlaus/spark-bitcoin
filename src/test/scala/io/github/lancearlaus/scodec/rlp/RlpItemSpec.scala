package io.github.lancearlaus.scodec.rlp

import java.nio.ByteBuffer

import org.scalatest.{Matchers, WordSpec}
import scodec.{DecodeResult, Attempt}
import scodec.bits._

class RlpItemSpec extends WordSpec with Matchers {

  // Drop leading zeroes
  def trimmedBytes(value: Long): Array[Byte] = {
    val buf = ByteBuffer.allocate(8).putLong(value).array()
    buf.slice(Math.min(buf.indexWhere(_ != 0), buf.size - 1), buf.size)
  }

  "Rlp decoder" should {

    "decode single byte string" in {
      for (b <- RlpItem.ByteStringRange) {
        val bits = BitVector.fromByte(b.toByte)
        val expected = RlpByteString(bits.bytes)

        RlpItem.codec.decode(bits) shouldBe Attempt.successful(DecodeResult(expected, BitVector.empty))
      }
    }

    "decode empty short string" in {
      val bits = BitVector.fromByte(RlpItem.ShortStringRange.start.toByte)
      val expected = RlpShortString(ByteVector.empty)

      RlpItem.codec.decode(bits) shouldBe Attempt.successful(DecodeResult(expected, BitVector.empty))
    }

    "decode single byte short strings" in {
      for (b <- (0x80 to 0xff)) {
        val bits = BitVector.fromValidHex(f"81$b%02x")
        val expected = RlpShortString(bits.bytes.drop(1))

        RlpItem.codec.decode(bits) shouldBe Attempt.successful(DecodeResult(expected, BitVector.empty))
      }
    }

    "decode 2 or more byte short strings" in {
      val lengthRange = 2 to RlpItem.ShortLengthMax
      val byteRange = 0 to 0xff

      for (len <- lengthRange; b <- byteRange) {
        val header = RlpItem.ShortStringRange.start + len
        val bits = BitVector(Stream(header.toByte) ++ Stream.fill(len)(b.toByte))
        val expected = RlpShortString(bits.bytes.drop(1))

        RlpItem.codec.decode(bits) shouldBe Attempt.successful(DecodeResult(expected, BitVector.empty))
      }
    }

    "decode long strings" in {
      val lengthRange = RlpItem.ShortLengthMax + 1 to RlpItem.ShortLengthMax + 100000 by 100
      val byteRange = 0 to 0xff by 128

      for (len <- lengthRange; b <- byteRange) {
        val lenBytes = trimmedBytes(len)
        val header = (RlpItem.LongStringRange.start - 1 + lenBytes.size).toByte
        val bits = BitVector(Iterator.single(header) ++ lenBytes.iterator ++ Iterator.fill(len)(b.toByte))
        val expected = RlpLongString(bits.bytes.drop(1 + lenBytes.size))

        RlpItem.codec.decode(bits) shouldBe Attempt.successful(DecodeResult(expected, BitVector.empty))
      }
    }

  }


}
