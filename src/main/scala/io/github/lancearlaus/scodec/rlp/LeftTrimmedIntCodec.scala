package io.github.lancearlaus.scodec.rlp

import java.lang.{Integer => javaInt}

import scodec.bits.BitVector
import scodec.codecs._
import scodec.{Attempt, Codec, DecodeResult, SizeBound}

// Trims leading zeros during encoding
// Must be wrapped with a variable length codec
object LeftTrimmedIntCodec extends Codec[Int] {

  override def sizeBound: SizeBound = SizeBound.bounded(8, javaInt.SIZE)

  override def encode(value: Int): Attempt[BitVector] =
    Attempt.successful(BitVector.fromInt(value, leftTrimmedBytesLength(value) * 8))

  override def decode(bits: BitVector): Attempt[DecodeResult[Int]] = uint(bits.length.toInt).decode(bits)
}
