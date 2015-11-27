package io.github.lancearlaus.scodec.rlp

import scodec._
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._


sealed trait RlpCodec[A] extends Codec[A]

object RlpCodec {

  // Wraps a normal codec
  private [rlp] def apply[A](codec: Codec[A]): RlpCodec[A] = new RlpCodec[A] {
    override def sizeBound: SizeBound = codec.sizeBound
    override def decode(bits: BitVector): Attempt[DecodeResult[A]] = codec.decode(bits)
    override def encode(value: A): Attempt[BitVector] = codec.encode(value)
  }

}

sealed trait RlpItem
case class RlpString(bits: BitVector) extends RlpItem
case class RlpList(items: List[RlpItem]) extends RlpItem


// Header byte representation for RLP items
case class RlpHeaderByte(header: Int) {
  require(header >= 0 && header <= 0xff, f"invalid header byte 0x$header%02x")

  def isValue       = (0x00 to 0x7f) contains header
  def isShortString = (0x80 to 0xb7) contains header
  def isLongString  = (0xb8 to 0xbf) contains header
  def isShortList   = (0xc0 to 0xf7) contains header
  def isLongList    = (0xf8 to 0xff) contains header

  def isString = isShortString || isLongString
  def isList   = isShortList   || isLongList
  def isShort  = isShortString || isShortList
  def isLong   = isLongString  || isLongList

  def value                  = if (isValue)       Some(header)        else None
  def shortStringLength      = if (isShortString) Some(header - 0x80) else None
  def longStringLengthLength = if (isLongString)  Some(header - 0xb7) else None
  def shortListLength        = if (isShortList)   Some(header - 0xc0) else None
  def longListLengthLength   = if (isLongList)    Some(header - 0xf7) else None

  def longLengthLength = longStringLengthLength orElse longListLengthLength
  def stringLength     = shortStringLength      orElse longStringLengthLength
  def listLength       = shortListLength        orElse longListLengthLength
}

object RlpHeaderByte {

  val codec = uint8.xmap[RlpHeaderByte](RlpHeaderByte.apply, _.header)

  def apply(value: RlpString): RlpHeaderByte = {
    val bytes = value.bits.bytes
    val length = bytes.length
    val first = if (length == 1) Some(bytes.get(0).toInt) else None

    first.filter(_ <= 0x7f).map(RlpHeaderByte.apply)                 // Single byte string
      .getOrElse { length match {
        case l if l <= 55 => RlpHeaderByte(0x80 + l)                 // Short string
        case l => RlpHeaderByte(0xb7 + leftTrimmedBytesLength(l))    // Long string
      }}
  }

  def apply(list: RlpList): RlpHeaderByte = list.items.length match {
    case l if l <= 55 => RlpHeaderByte(0xc0 + l)
    case l => RlpHeaderByte(0xf7 + leftTrimmedBytesLength(l))
  }

  def apply(item: RlpItem): RlpHeaderByte = item match {
    case s: RlpString => apply(s)
    case l: RlpList => apply(l)
  }

}

// Pattern matching helper objects

object ValueHeader       { def unapply(hb: RlpHeaderByte) = hb.value }
object StringHeader      { def unapply(hb: RlpHeaderByte) = hb.isString }
object ListHeader        { def unapply(hb: RlpHeaderByte) = hb.isList }
object ShortStringHeader { def unapply(hb: RlpHeaderByte) = hb.shortStringLength }
object LongStringHeader  { def unapply(hb: RlpHeaderByte) = hb.longStringLengthLength }
object ShortListHeader   { def unapply(hb: RlpHeaderByte) = hb.shortListLength }
object LongListHeader    { def unapply(hb: RlpHeaderByte) = hb.longListLengthLength }



object RlpItem {

  def codec: Codec[RlpItem] = lazily {

    def encode(item: RlpItem): Attempt[BitVector] = item match {
      case str: RlpString => RlpString.codec.encode(str)
      case list: RlpList => RlpList.codec.encode(list)
    }

    def decode(buffer: BitVector): Attempt[DecodeResult[RlpItem]] = for {
      hb <- RlpHeaderByte.codec.decode(buffer)
      decoded <- hb.value match {
        case ValueHeader(_) | StringHeader() => RlpString.decodeFromHeader(hb)
        case ListHeader() => RlpList.codec.decode(buffer)
      }
    } yield decoded

    Codec[RlpItem](encode _, decode _)
  }

}

object RlpString {

  def apply(n: Byte): RlpString = RlpString(BitVector.fromByte(n))
  def apply(n: Int): RlpString = RlpString(BitVector.fromInt(n, leftTrimmedBytesLength(n) * 8))
  def apply(n: Long): RlpString = RlpString(BitVector.fromLong(n, leftTrimmedBytesLength(n) * 8))

  val codec: Codec[RlpString] = {

    def encode(str: RlpString): Attempt[BitVector] = {
      val hb = RlpHeaderByte(str)
      for {
        header <- RlpHeaderByte.codec.encode(hb)
        encoded <- hb match {
          case ValueHeader(v) => Attempt.successful(header)
          case ShortStringHeader(len) => Attempt.successful(header ++ str.bits)
          case LongStringHeader(lenlen) => for {
            len <- ulong(lenlen * 8).encode(str.bits.bytes.length)
          } yield header ++ len ++ str.bits
        }
      } yield encoded
    }

    def decode(buffer: BitVector): Attempt[DecodeResult[RlpString]] = for {
      hb <- RlpHeaderByte.codec.decode(buffer)
      decoded <- decodeFromHeader(hb)
    } yield decoded

    Codec[RlpString](encode _, decode _)
  }

  def decodeFromHeader(hb: DecodeResult[RlpHeaderByte]): Attempt[DecodeResult[RlpString]] = for {
    decoded <- hb.value match {
      case ValueHeader(v) => Attempt.successful(DecodeResult(RlpString(BitVector.fromInt(v, 8)), hb.remainder))
      case ShortStringHeader(len) => codecs.bits(len * 8).decode(hb.remainder).map(_.map(RlpString.apply))
      case LongStringHeader(lenlen) => for {
        len <- ulong(lenlen * 8).decode(hb.remainder)
        str <- codecs.bits(len.value.toInt * 8).decode(len.remainder).map(_.map(RlpString.apply))
      } yield str
    }
  } yield decoded


}

object RlpList {

  def codec: Codec[RlpList] = {

    def encode(list: RlpList): Attempt[BitVector] = {
      val hb = RlpHeaderByte(list)
      for {
        header <- RlpHeaderByte.codec.encode(hb)
        encoded <- hb match {
          case ShortListHeader(len) =>
            listOfN(shortLengthCodec(0xc0), RlpItem.codec).encode(list.items)
          case LongListHeader(lenlen) =>
            listOfN(longLengthCodec(0xf7).xmap(_.toInt, _.toLong), RlpItem.codec).encode(list.items)
        }
      } yield encoded
    }

    def decode(buffer: BitVector): Attempt[DecodeResult[RlpList]] = for {
      hb <- RlpHeaderByte.codec.decode(buffer)
      decoded <- hb.value match {
        case ShortListHeader(len) =>
          listOfN(shortLengthCodec(0xc0), RlpItem.codec).decode(buffer)
            .map(_.map(RlpList.apply))
        case LongListHeader(lenlen) =>
          listOfN(longLengthCodec(0xf7).xmap(_.toInt, _.toLong), RlpItem.codec).decode(buffer)
            .map(_.map(RlpList.apply))
      }
    } yield decoded

    Codec[RlpList](encode _, decode _)
  }

}