import java.nio.channels.FileChannel
import java.nio.file.{Paths, StandardOpenOption}

import io.github.lancearlaus.bitcoin.BlockChain
import scodec.bits.BitVector

import scala.collection.mutable


object Main {

  val magic = 0xD9B4BEF9L
  val version = 1
  val blockChain = BlockChain(magic, version)

  def decodeEntries(bits: BitVector): List[BlockChain#BlockEntry] = {
    val entries = mutable.MutableList.empty[BlockChain#BlockEntry]
    var count = 0
    blockChain.entries(bits).foreach { result =>
      result.fold(
        err => println(s"ERROR: $err"),
        { entry =>
          entries += entry
          count += 1
          if (count % 100 == 0) print(".")
          if (count % 10000 == 0) println()
        }
      )
    }
    println()
    println(s"Total entries: $count")

    entries.toList
  }

  def main(args: Array[String]) {

    val fileName = "data/bootstrap.dat"
//    val fileName = "data/blk00000.dat"

    println(s"Opening file $fileName")

    val channel = FileChannel.open(Paths.get(fileName), StandardOpenOption.READ)
    val bits = BitVector.fromMmap(channel)

    println(f"Decoding file $fileName%s (size: ${bits.size / 8}%,.0f)")
    val entries = decodeEntries(bits)

    for (i <- 0 to 100) {
      println(s"${entries(i).block(bits)}")
    }

  }
}