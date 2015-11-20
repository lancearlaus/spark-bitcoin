package io.github.lancearlaus.bitcoin

import io.github.yzernik.bitcoinscodec.messages.Block
import scodec.Attempt.{Failure, Successful}
import scodec.{Decoder, Codec}
import scodec.bits.{BitVector, ByteOrdering}
import scodec.codecs._
import scala.annotation.tailrec
import scala.collection.mutable

case class Blockchain(magic: Long, version: Int) {

  val blockCodec = Block.codec(version)

  case class BlockHeader(length: Long)

  object BlockHeader {
    val length = 8   // bytes
    val codec = (
      constant(BitVector.fromLong(magic, 32, ByteOrdering.LittleEndian)) :~>:
      ("length" | uint32L)
    ).as[BlockHeader]
  }

  object BlockHeaders {

    // Decodes headers without decoding blocks
    val decoder = Decoder[BlockHeader] { data: BitVector =>
      for {
        header <- BlockHeader.codec.decode(data)
        skip <- bits(header.value.length * 8).decode(header.remainder)
      } yield header.mapRemainder(r => skip.remainder)
    }

  }

  case class BlockEntry(offset: Long, length: Long) {
    def end = offset + length
  }
  object BlockEntry {
    def apply(offset: Long, header: BlockHeader): BlockEntry = BlockEntry(offset + BlockHeader.length, header.length)
  }


  val codec: Codec[Block] = {
    def encode(block: Block) = for {
      payload <- blockCodec.encode(block)
      header <- BlockHeader.codec.encode(BlockHeader(payload.length / 8))
    } yield header ++ payload

    def decode(bits: BitVector) = for {
      header <- BlockHeader.codec.decode(bits)
      // Split to ensure block decode doesn't consume more bits than stated
      (payload, remainder) = header.remainder.splitAt(header.value.length * 8)
      block <- blockCodec.decode(payload)
    } yield block.mapRemainder(_ => remainder)

    Codec[Block](encode _, decode _)
  }

  // Extract all block locations (without parsing the blocks
  def entries(bits: BitVector): Either[String, List[BlockEntry]] = {

    // TODO: Figure out why this recursive function leads to OutOfMemoryError
    @tailrec
    def decode(bits: BitVector, offset: Long, entries: mutable.MutableList[BlockEntry] = mutable.MutableList.empty): Either[String, List[BlockEntry]] = {
      if (bits.nonEmpty) {
        (for {
          header <- BlockHeaders.decoder.decode(bits)
//          entry =
        } yield (BlockEntry(offset, header.value), header.remainder)) match {
          case Successful((entry, tail)) => decode(tail, entry.end, entries += entry)
          case Failure(cause) => Left(cause.messageWithContext)
        }
      } else {
        Right(entries.toList)
      }
    }
    //    decode(bits, 0)

    var offset = 0L
    var tail = bits
    val entries = mutable.MutableList.empty[BlockEntry]

    while (tail.nonEmpty) {
      BlockHeaders.decoder.decode(tail) match {
        case Successful(result) => {
          val entry = BlockEntry(offset, result.value)
          entries += entry
          tail = result.remainder
          offset = entry.end
        }
        case Failure(cause) => {
          println(s"ERROR: ${cause.messageWithContext}")
          tail = BitVector.empty
        }
      }
    }

    Right(entries.toList)
  }

}
