package io.github.lancearlaus.scodec.rlp

import scodec._
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._
import shapeless._

sealed trait RlpItem

sealed trait RlpString extends RlpItem {
  val bytes: ByteVector
  def bits = bytes.bits
}

case class RlpByteString private[rlp] (bytes: ByteVector) extends RlpString {
  require(bytes.size <= 1, s"invalid byte vector (size: ${bytes.size})")
}
case class RlpShortString private[rlp] (bytes: ByteVector) extends RlpString {
  require(bytes.size <= RlpItem.ShortLengthMax, s"invalid byte vector (size: ${bytes.size})")
}
case class RlpLongString private[rlp] (bytes: ByteVector) extends RlpString


object RlpString {

  def apply(bytes: ByteVector): RlpString = apply(bytes.bits)

  def apply(bits: BitVector): RlpString = {
    val bytes = bits.bytes
    bytes.size match {
      case 1 if RlpItem.ByteStringRange.contains(bytes(0)) => RlpByteString(bytes)
      case s if s <= RlpItem.ShortLengthMax => RlpShortString(bytes)
      case _ => RlpLongString(bytes)
    }
  }

}

sealed trait RlpList extends RlpItem {
  val length: Long
  val bytes: ByteVector
  def bits = bytes.bits
  def items: Iterator[RlpItem]
}

//case class RlpShortList private[rlp] (length: Long, bytes: ByteVector) extends RlpList {
//  require(length <= RlpItem.ShortLengthMax, s"invalid byte vector (size: ${bytes.size})")
//}


object RlpItem {

  // Maximum length of a short string or list
  val ShortLengthMax = 55

  // Header value ranges
  val ByteStringRange  = 0x00 to 0x7f
  val ShortStringRange = 0x80 to 0xb7
  val LongStringRange  = 0xb8 to 0xbf
  val ShortListRange   = 0xc0 to 0xf7
  val LongListRange    = 0xf8 to 0xff

  // Length codecs
  def shortLengthCodec(base: Int) = uint8.xmap[Int](_ - base, _ + base)
  def longLengthCodec(base: Int) = variableSizeBytes(shortLengthCodec(base), LeftTrimmedLongCodec)

  // String codecs
  val byteStringCodec = bytes(1).xmap[RlpByteString](RlpByteString.apply, _.bytes)
  val shortStringCodec = variableSizeBytes(shortLengthCodec(ShortStringRange.start), bytes).xmap[RlpShortString](RlpShortString.apply, _.bytes)
  val longStringCodec = variableSizeBytesLong(longLengthCodec(LongStringRange.start - 1), bytes).xmap[RlpLongString](RlpLongString.apply, _.bytes)

  // List codecs
  val shortListCodec = variableSizeBytes(shortLengthCodec(ShortListRange.start), bytes)

  val codec = {

    def decode(bits: BitVector): Attempt[DecodeResult[RlpItem]] = {
      uint8.decode(bits).flatMap( _.value  match {
        case h if ByteStringRange.contains(h) => byteStringCodec.decode(bits)
        case h if ShortStringRange.contains(h) => shortStringCodec.decode(bits)
        case h if LongStringRange.contains(h) => longStringCodec.decode(bits)
        case h => Attempt.failure(Err(s"no codec found for header value ${h.toHexString}"))
      })
    }

    def encode(item: RlpItem): Attempt[BitVector] = item match {
      case v: RlpByteString => byteStringCodec.encode(v)
      case v: RlpShortString => shortStringCodec.encode(v)
      case v: RlpLongString => longStringCodec.encode(v)
    }

    Codec[RlpItem](encode _, decode _)
  }


  sealed trait HeaderType {
    def header: Int
    def isLongHeader = false
  }
  sealed trait LongHeader extends HeaderType {
    def lengthLength: Int
    override def isLongHeader = true
  }

  case class ByteString(value: Int) extends HeaderType { def header = value }
  case class ShortString(length: Int) extends HeaderType { def header = length + 5 }
  case class LongString(lengthLength: Int) extends HeaderType with LongHeader { def header = lengthLength + 10 }

  object HeaderType {
    implicit val codec = uint8.xmap[HeaderType](HeaderType.apply, _.header)

    def apply(header: Int): HeaderType = header match {
      case h if h >= 0 && h <= 5 => ByteString(h)
      case h if h > 5 && h <= 10 => ShortString(h - 5)
    }
  }


  def isLongItem(first: Int) = false
  def longItemLengthLength(first: Int) = 2

  type TestHeader = HeaderType :: Option[Long] :: HNil
  val testHeaderCodec: Codec[TestHeader] = {
    ("header type" | HeaderType.codec) >>:~ { headerType =>
    ("long length" | conditional(headerType.isLongHeader, ulong(headerType.asInstanceOf[LongHeader].lengthLength * 8))).hlist
  }}.as[TestHeader]

  val testStringCodec: Codec[TestRlpString] = { testHeaderCodec >>:~ { header => header match {
    case ByteString(value) :: _ :: HNil => ignore(0).xmap[TestRlpString](_ => TestRlpString(BitVector.fromInt(value, 8)), _ => ()).hlist
    case ShortString(length) :: _ :: HNil => codecs.bits(length * 8).xmap[TestRlpString](bits => TestRlpString(bits), _.bits).hlist
    case LongString(_) :: Some(length) :: HNil => codecs.bits(length * 8).xmap[TestRlpString](bits => TestRlpString(bits), _.bits).hlist
    case LongString(length) :: None :: HNil => codecs.bits(length * 8).xmap[TestRlpString](bits => TestRlpString(bits), _.bits).hlist
  }}}.as[TestRlpString]

  val testByteStringCodec: Codec[TestRlpString] = uint8.exmap[TestRlpString](
    _ match {
      case v if (v >= 0 && v <= 5) => Attempt.successful(TestRlpString(BitVector.fromInt(v, 8)))
      case v => Attempt.failure(Err(s"bad value $v"))
    },
    _ match {
      case s if (s.bits.length == 8) => uint8.decode(s.bits).flatMap( _ match {
        case r if (r.value >= 0 && r.value <= 5) => Attempt.successful(r.value)
        case r => Attempt.failure(Err(s"bad value ${r.value}"))
      })
      case s => Attempt.failure(Err("string too long"))
    }
  )

//  sealed trait TestHeader
//  case object TestValueHeader extends TestHeader
//  case object TestStringHeader extends TestHeader
//  case object TestListHeader extends TestHeader
//
//
//  val testHeaderCodec = uint8.xmap[TestHeader](
//    header => header match {
//      case v if v <= 10 => TestValueHeader
//    },
//    header => header match {
//      case TestValueHeader => 10
//    }
//  )
//

}



sealed trait TestRlpItem
case class TestRlpString(bits: BitVector) extends TestRlpItem
case class TestRlpList(length: Long, bits: BitVector) extends TestRlpItem


object RlpListCodec extends Codec[TestRlpList] {

  override def sizeBound: SizeBound = SizeBound.unknown

  override def encode(value: TestRlpList): Attempt[BitVector] = ???

  override def decode(bits: BitVector): Attempt[DecodeResult[TestRlpList]] = for {
    length <- rlpListLength.decode(bits)
  } yield length.map(l => TestRlpList(l, length.remainder))

}

class RlpStringCodec[T] extends Codec[TestRlpString] {

  val SingleByteValueRange  = 0x00 to 0x7f

  override def sizeBound: SizeBound = SizeBound.unknown

  override def encode(value: TestRlpString): Attempt[BitVector] = {
    val bytes = value.bits.bytes
    bytes.size match {
      case 1 if SingleByteValueRange.contains(bytes(0)) => Attempt.successful(value.bits)
      case _ => for (length <- rlpStringLength.encode(bytes.length)) yield length ++ value.bits
    }
  }

  override def decode(bits: BitVector): Attempt[DecodeResult[TestRlpString]] = {
    uint8.decode(bits).flatMap(header => header.value match {
      case h if SingleByteValueRange.contains(h) =>
        Attempt.successful(DecodeResult(TestRlpString(bits.take(8)), header.remainder))
      case _ => for {
        length <- rlpStringLength.decode(bits)
        buffer <- bytes(length.value.toInt).decode(length.remainder)
      } yield buffer.map(bytes => TestRlpString(bytes.bits))
    })
  }

}


case class RlpLengthCodec(base: Int) extends Codec[Long] {
  import RlpLengthCodec._

  val ShortLengthMax = 55
  val shortCodec = shortLengthCodec(base)
  val longCodec = longLengthCodec(base + ShortLengthMax)

  override def sizeBound: SizeBound = SizeBound.bounded(1, 9) * 8

  override def encode(length: Long): Attempt[BitVector] = length match {
    case l if (l >= 0 && l <= ShortLengthMax) => shortCodec.encode(l.toInt)
    case l if (l > ShortLengthMax) => longCodec.encode(l.toInt)
    case l => Attempt.failure(Err(s"invalid length $l"))
  }

  override def decode(buffer: BitVector): Attempt[DecodeResult[Long]] = {
    uint8.decode(buffer).flatMap { header =>
      header.value match {
        case l if (l >= base && l <= base + ShortLengthMax) => shortCodec.decode(buffer).map(_.map(_.toLong))
        case l if (l > base + ShortLengthMax && l <= base + ShortLengthMax + 8) => longCodec.decode(buffer)
        case h => Attempt.failure(Err(s"invalid header 0x${h.toHexString}"))
      }
    }
  }

}

object RlpLengthCodec {
  private def shortLengthCodec(base: Int) = uint8.xmap[Int](_ - base, _ + base)
  private def longLengthCodec(base: Int) = variableSizeBytes(shortLengthCodec(base), LeftTrimmedLongCodec)
}
