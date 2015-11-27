package io.github.lancearlaus.scodec

import scodec.codecs._
import java.lang.{Integer => javaInt}

package object rlp {

  // Length codecs
  private[rlp] def shortLengthCodec(base: Int) = uint8.xmap[Int](_ - base, _ + base)
  private[rlp] def longLengthCodec(base: Int) = variableSizeBytes(shortLengthCodec(base), LeftTrimmedLongCodec)

  def leftTrimmedBytesLength(value: Int) = Math.max(javaInt.SIZE - javaInt.numberOfLeadingZeros(value), 8) / 8

  implicit val rlpItem = RlpItem.codec

  implicit val rlpInt = LeftTrimmedIntCodec
  implicit val rlpLong = LeftTrimmedLongCodec

}
