package io.github.lancearlaus.bitcoin

import io.github.yzernik.bitcoinscodec.messages.Block
import scodec.Codec
import scodec.bits.{BitVector, ByteOrdering}
import scodec.codecs._


object Blockchain {

  def codec(magic: Long, version: Int): Codec[Block] = {

    val magicCodec = constant(BitVector.fromLong(magic, 32, ByteOrdering.LittleEndian))
    val blockCodec = Block.codec(version)

    def encode(block: Block) = for {
      magic <- magicCodec.encode(())
      payload <- blockCodec.encode(block)
      length <- uint32L.encode(payload.size / 8)
    } yield magic ++ length ++ payload

    def decode(data: BitVector) = for {
      magic <- magicCodec.decode(data)
      length <- uint32L.decode(magic.remainder)
      payload <- bits(length.value * 8).decode(length.remainder)
      block <- blockCodec.decode(payload.value)
    } yield block.mapRemainder(_ => payload.remainder)

    Codec[Block](encode _, decode _)
  }

}
