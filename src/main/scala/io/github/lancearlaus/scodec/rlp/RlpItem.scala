package io.github.lancearlaus.scodec.rlp

import scodec._
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._



sealed trait RlpItem
case class RlpString(bits: BitVector) extends RlpItem
case class RlpList(items: List[RlpItem]) extends RlpItem


// Header byte representation for RLP items
case class HeaderByte(header: Int) {
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

object HeaderByte {

  val codec = uint8.xmap[HeaderByte](HeaderByte.apply, _.header)

  def apply(value: RlpString): HeaderByte = {
    val bytes = value.bits.bytes
    val length = bytes.length
    val first = if (length == 1) Some(bytes.get(0).toInt) else None

    first.filter(_ <= 0x7f)
      .map(HeaderByte.apply)                                      // Single byte string
      .getOrElse { length match {
        case l if l <= 55 => HeaderByte(0x80 + l)                 // Short string
        case l => HeaderByte(0xb7 + leftTrimmedBytesLength(l))    // Long string
      }}
  }

  def apply(list: RlpList): HeaderByte = list.items.length match {
    case l if l <= 55 => HeaderByte(0xc0 + l)
    case l => HeaderByte(0xf7 + leftTrimmedBytesLength(l))
  }

  def apply(item: RlpItem): HeaderByte = item match {
    case s: RlpString => apply(s)
    case l: RlpList => apply(l)
  }

}

// Pattern matching helper objects

object ValueHeader       { def unapply(hb: HeaderByte) = hb.value }
object ShortStringHeader { def unapply(hb: HeaderByte) = hb.shortStringLength }
object LongStringHeader  { def unapply(hb: HeaderByte) = hb.longStringLengthLength }
object ShortListHeader   { def unapply(hb: HeaderByte) = hb.shortListLength }
object LongListHeader    { def unapply(hb: HeaderByte) = hb.longListLengthLength }



object RlpItem {

  def codec: Codec[RlpItem] = lazily {

    def encode(item: RlpItem): Attempt[BitVector] = {
      val hb = HeaderByte(item)
      for {
        header <- HeaderByte.codec.encode(hb)
        encoded <- item match {
          case str: RlpString => hb match {
            case ValueHeader(v) => Attempt.successful(header)
            case ShortStringHeader(len) => Attempt.successful(header ++ str.bits)
            case LongStringHeader(lenlen) => for {
              len <- ulong(lenlen * 8).encode(str.bits.bytes.length)
            } yield header ++ len ++ str.bits
          }
          case list: RlpList => hb match {
            case ShortListHeader(len) =>
              listOfN(shortLengthCodec(0xc0), codec).encode(list.items)
            case LongListHeader(lenlen) =>
              listOfN(longLengthCodec(0xf7).xmap(_.toInt, _.toLong), codec).encode(list.items)
          }
        }
      } yield encoded
    }

    def decode(buffer: BitVector): Attempt[DecodeResult[RlpItem]] = for {
      hb <- HeaderByte.codec.decode(buffer)
      decoded <- hb.value match {
        case ValueHeader(v) => Attempt.successful(DecodeResult(RlpString(BitVector.fromInt(v, 8)), hb.remainder))
        case ShortStringHeader(len) => codecs.bits(len * 8).decode(hb.remainder).map(_.map(RlpString.apply))
        case LongStringHeader(lenlen) => for {
          len <- ulong(lenlen * 8).decode(hb.remainder)
          str <- codecs.bits(len.value.toInt * 8).decode(len.remainder).map(_.map(RlpString.apply))
        } yield str
        case ShortListHeader(len) =>
          listOfN(shortLengthCodec(0xc0), codec).decode(buffer)
            .map(_.map(RlpList.apply))
        case LongListHeader(lenlen) =>
          listOfN(longLengthCodec(0xf7).xmap(_.toInt, _.toLong), codec).decode(buffer)
            .map(_.map(RlpList.apply))
      }
    } yield decoded

    Codec[RlpItem](encode _, decode _)
  }

}
