package io.github.lancearlaus.scodec

import java.nio.charset.Charset

import scodec.bits.BitVector
import scodec.{Attempt, Codec}
import scodec.codecs._
import java.lang.{Integer => javaInt, Long => javaLong}

package object rlp {

  // Length codecs
  private[rlp] def shortLengthCodec(base: Int) = uint8.xmap[Int](_ - base, _ + base)
  private[rlp] def longLengthCodec(base: Int) = variableSizeBytes(shortLengthCodec(base), LeftTrimmedLongCodec)

  def leftTrimmedBytesLength(value: Int) = Math.max(javaInt.SIZE - javaInt.numberOfLeadingZeros(value), 8) / 8
  def leftTrimmedBytesLength(value: Long) = Math.max(javaLong.SIZE - javaLong.numberOfLeadingZeros(value), 8) / 8

  // Raw RLP codecs
  val rlpItem = RlpItem.codec
  val rlpItemString = RlpString.codec
  val rlpItemList = RlpList.codec


  private[rlp] def rlpCodec[A](codec: Codec[A]) = RlpCodec(codec)

  val rlpByte: RlpCodec[Byte] = rlpCodec(RlpString.codec.narrow[Byte](
    s => byte(s.bits.length.toInt).decode(s.bits).map(_.value),
    RlpString.apply
  ))

  val rlpInt: RlpCodec[Int] = rlpCodec(RlpString.codec.narrow[Int](
    s => LeftTrimmedIntCodec.decode(s.bits).map(_.value),
    RlpString.apply
  ))

  val rlpLong: RlpCodec[Long] = rlpCodec(RlpString.codec.narrow[Long](
    s => LeftTrimmedLongCodec.decode(s.bits).map(_.value),
    RlpString.apply
  ))

  def rlpString(implicit charset: Charset): Codec[String] = RlpString.codec.narrow[String](
    s => string.decode(s.bits).map(_.value),
    s => RlpString.apply(BitVector(s.getBytes(charset)))
  )

//  def rlpList[A](codec: RlpCodec[A]): Codec[List[A]] = RlpList.codec.narrow[List[A]](
//    l => list(codec),
//    l => RlpList.apply
//  )
//  def rlpList2[A](codec: RlpCodec[A]): Codec[List[A]] = RlpList.codec.exmap[List[A]](
//    l => list() l.items,
//    l => RlpList.apply
//  )
}
