package io.github.lancearlaus.bitcoin

import io.github.yzernik.bitcoinscodec.messages.Block
import scodec.Attempt.{Failure, Successful}
import scodec.bits.{BitVector, ByteOrdering}
import scodec.codecs._
import scodec.{Codec, Err}

import scala.collection.mutable

case class Blockchain(magic: Long, version: Int) {

  val blockCodec = Block.codec(version)

  val codec: Codec[Block] = (
    constant(BitVector.fromLong(magic, 32, ByteOrdering.LittleEndian)) ~>
      variableSizeBytesLong(uint32L, blockCodec)
    )


  case class BlockHeader(length: Long)

  object BlockHeader {
    val length = 8   // bytes

    // Codec that only extracts the header (skips the payload)
    val codec: Codec[BlockHeader] = (
      constant(BitVector.fromLong(magic, 32, ByteOrdering.LittleEndian)) ~>
        ("length" | uint32L.flatPrepend(length => ignore(length * 8).hlist.dropUnits))
    ).as[BlockHeader]
  }

  case class BlockChunk(offset: Long, length: Long) {
    def end = offset + length
    def decode(bits: BitVector) = blockCodec.decode(bits.drop(offset).take(length))
  }
  object BlockChunk {
    def apply(offset: Long, header: BlockHeader): BlockChunk = BlockChunk(offset + BlockHeader.length, header.length)
  }

  def chunks(bits: BitVector): Either[Err, List[BlockChunk]] = {
    var offset = 0L
    var tail = bits
    val chunks = mutable.MutableList.empty[BlockChunk]

    while (tail.nonEmpty) {
      BlockHeader.codec.decode(tail) match {
        case Successful(result) => {
          val chunk = BlockChunk(offset, result.value)
          chunks += chunk
          tail = result.remainder
          offset = chunk.end
        }
        case Failure(cause) => {
          println(s"ERROR: ${cause.messageWithContext}")
          return Left(cause)
        }
      }
    }

    Right(chunks.toList)
  }

}
