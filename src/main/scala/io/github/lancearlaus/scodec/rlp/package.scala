package io.github.lancearlaus.scodec

package object rlp {

  val rlpStringLength = RlpLengthCodec(0x80)

  val rlpListLength = RlpLengthCodec(0xc0)

}
