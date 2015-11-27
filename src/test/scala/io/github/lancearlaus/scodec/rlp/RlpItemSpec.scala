package io.github.lancearlaus.scodec.rlp

import java.nio.ByteBuffer
import java.nio.charset.Charset

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
      for (b <- (0x00 to 0x7f)) {
        val bits = BitVector(b)
        val expected = RlpString(bits)

        RlpItem.codec.decode(bits) shouldBe Attempt.successful(DecodeResult(expected, BitVector.empty))
      }
    }

    "decode empty short string" in {
      val bits = BitVector(0x80)
      val expected = RlpString(BitVector.empty)

      RlpItem.codec.decode(bits) shouldBe Attempt.successful(DecodeResult(expected, BitVector.empty))
    }

    "decode single byte short strings" in {
      for (b <- (0x80 to 0xff)) {
        val bits = BitVector(0x80 + 1, b)
        val expected = RlpString(bits.drop(8))

        RlpItem.codec.decode(bits) shouldBe Attempt.successful(DecodeResult(expected, BitVector.empty))
      }
    }

    "decode 2 or more byte short strings" in {
      val lengthRange = 2 to 55
      val byteRange = 0 to 0xff

      for (len <- lengthRange; b <- byteRange) {
        val header = 0x80 + len
        val bits = BitVector(Stream(header.toByte) ++ Stream.fill(len)(b.toByte))
        val expected = RlpString(bits.drop(8))

        RlpItem.codec.decode(bits) shouldBe Attempt.successful(DecodeResult(expected, BitVector.empty))
      }
    }

//    "decode long strings" in {
//      val lengthRange = 0xb7 + 1 to 0xb7 + 100000 by 100
//      val byteRange = 0 to 0xff by 128
//
//      for (len <- lengthRange; b <- byteRange) {
//        val lenBytes = trimmedBytes(len)
//        val header = (0xb8 - 1 + lenBytes.size).toByte
//        val bits = BitVector(Iterator.single(header) ++ lenBytes.iterator ++ Iterator.fill(len)(b.toByte))
//        val expected = RlpString(bits.drop((1 + lenBytes.size) * 8))
//
//        RlpItem.codec.decode(bits) shouldBe Attempt.successful(DecodeResult(expected, BitVector.empty))
//      }
//    }

  }

  "RLP codec" should {

    "roundtrip an empty list" in {
      val bits = BitVector(0xc0)
      val expected = RlpList(List.empty)

      RlpItem.codec.decode(bits) shouldBe Attempt.successful(DecodeResult(expected, BitVector.empty))
      RlpItem.codec.decode(bits).flatMap(r => RlpItem.codec.encode(r.value)) shouldBe Attempt.successful(bits)
    }

    "roundtrip a short list" in {
      val bits = BitVector(0xc3, 0x00, 0x01, 0x02)
      val expected = RlpList(List(0, 1, 2).map(n => RlpString(BitVector(n))))

      RlpItem.codec.decode(bits) shouldBe Attempt.successful(DecodeResult(expected, BitVector.empty))
      RlpItem.codec.decode(bits).flatMap(r => RlpItem.codec.encode(r.value)) shouldBe Attempt.successful(bits)
    }

  }

  case class CodecTest(n: Int, l: Long, s: String)

  implicit val charSet = Charset.defaultCharset()

  val testCodec = (rlpInt :: rlpLong :: rlpString).as[CodecTest]

}
