package io.github.lancearlaus.bitcoin

import io.github.yzernik.bitcoinscodec.messages.Block
import scodec.bits.{BitVector, ByteOrdering}
import scodec.codecs._
import scodec.{Codec, Err}

case class BlockChain(magic: Long, version: Int) {

  private val magicCodec = constant(BitVector.fromLong(magic, 32, ByteOrdering.LittleEndian))
  private val blockCodec = Block.codec(version)

  val codec: Codec[Block] = magicCodec ~> variableSizeBytesLong(uint32L, blockCodec)


  case class BlockHeader(length: Long)

  object BlockHeader {
    val length = 8   // bytes

    // Codec that only extracts the header (skips the payload)
    val codec: Codec[BlockHeader] = (
      magicCodec ~> ("length" | uint32L.flatPrepend(length => ignore(length * 8).hlist.dropUnits))
    ).as[BlockHeader]
  }

  case class BlockEntry(offset: Long, length: Long) {
    def end = offset + length
    /**
     * Retrieves the block represented by this entry from the chain
     *
     * @param chain BitVector of the entire chain (underlying storage must support random access)
     * @return the decoded block or corresponding decoding error
     */
    def block(chain: BitVector): Either[Err, Block] =
      blockCodec.decode(chain.drop(offset * 8).take(length * 8)).toEither.right.map(_.value)
  }
  object BlockEntry {
    def apply(offset: Long, header: BlockHeader): BlockEntry = BlockEntry(offset + BlockHeader.length, header.length)
  }

  def entries(bits: BitVector) = new Iterator[Either[Err, BlockEntry]] {
    var offset = 0L
    var tail = bits

    override def hasNext: Boolean = tail.nonEmpty

    override def next(): Either[Err, BlockEntry] = {
      BlockHeader.codec.decode(tail).toEither.right.map { result =>
        val chunk = BlockEntry(offset, result.value)
        tail = result.remainder
        offset = chunk.end
        chunk
      }
    }
  }

//  def entries2(bits: BitVector): Either[Err, List[BlockEntry]] = {
//
//    // The following fails with OutOfMemoryError on large files
//    @tailrec
//    def decode(bits: BitVector, offset: Long = 0, entries: mutable.MutableList[BlockEntry] = mutable.MutableList.empty): Either[Err, List[BlockEntry]] = {
//      if (bits.nonEmpty) {
//        BlockHeader.codec.decode(bits).toEither.right.map(_.map(h => BlockEntry(offset, h))) match {
//          case Left(err) => Left(err)
//          case Right(DecodeResult(entry, remainder)) => decode(remainder, entry.end, entries += entry)
//        }
//      } else {
//        Right(entries.toList)
//      }
//    }
//
//    decode(bits)
//  }

}
