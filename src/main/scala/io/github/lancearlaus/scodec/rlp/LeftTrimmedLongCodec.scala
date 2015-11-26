package io.github.lancearlaus.scodec.rlp

import java.lang.{Long => javaLong}

import scodec.bits.BitVector
import scodec.codecs._
import scodec.{Attempt, Codec, DecodeResult, SizeBound}

// Trims leading zeros during encoding
// Must be wrapped with a variable length codec
object LeftTrimmedLongCodec extends Codec[Long] {

  override def sizeBound: SizeBound = SizeBound.bounded(8, javaLong.SIZE)

  override def encode(value: Long): Attempt[BitVector] = {
    val leadingZeroBytes = javaLong.numberOfLeadingZeros(value) / 8
    val bits = Math.max(1, javaLong.BYTES - leadingZeroBytes) * 8
    Attempt.successful(BitVector.fromLong(value, bits))
  }

  override def decode(bits: BitVector): Attempt[DecodeResult[Long]] = ulong(bits.size.toInt).decode(bits)
}
