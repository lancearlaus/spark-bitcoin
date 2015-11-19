import java.io.FileInputStream

import io.github.lancearlaus.bitcoin.Blockchain
import scodec.Attempt.{Failure, Successful}
import scodec.bits.BitVector


object Main {

  def main(args: Array[String]) {

    val magic = 0xD9B4BEF9L
    val version = 1
    //    val file = "data/bootstrap.dat"
    val file = "data/blk00000.dat"

    val codec = Blockchain.codec(magic, version)
    val stream = new FileInputStream(file)
    val bits = BitVector.fromInputStream(stream)

    println(s"Loading file $file...")

    var (blockCount, txCount) = (0L, 0L)
    var remainder = bits

    while (remainder.nonEmpty) {
      codec.decode(remainder) match {
        case Successful(result) => {
          remainder = result.remainder
          blockCount = blockCount + 1
          //txCount = txCount + result.value.block.txs.size
          //          println(s"count: $count, frame: ${result.value}")
          print(".")
          if (blockCount % 100 == 0) println(s" -> blocks: $blockCount, txs: $txCount")
        }
        case Failure(cause) => {
          println(s"FAILED: $cause")
          remainder = BitVector.empty
        }
      }
    }

    println(s"Totals: blocks: $blockCount, txs: $txCount")

  }

}