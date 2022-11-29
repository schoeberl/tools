package teaching

object CacheSim extends App {
  // Those three are varied for different version and the access list be, nothing else
  val wordOffsetBits = 3
  val indexBits = 8

  val byteOffsetBits = 2
  val lineSize = math.pow(2, (wordOffsetBits + byteOffsetBits)).toInt
  val nrLines = math.pow(2, indexBits).toInt
  val size = lineSize * nrLines
  val tagBits = 32 - indexBits - wordOffsetBits - byteOffsetBits
  println(s"Given: line/block size is $lineSize bytes, cache is $size bytes")
  println(s"Request: Number of lines $nrLines")
  println(s"fields: byte offset is 2, word offset is $wordOffsetBits, index bits is $indexBits, tag bits is $tagBits")

  // first is address in blocks/lines, second offset within block in bytes
  val accessList = List((3, 2), (5, 1), (3, 4), (5 + 2 * nrLines, 5), (5, 1), (7 + 5 * nrLines, 5),
    (3 + 4 * nrLines, 7), (3, 5), (7 + 5 * nrLines, 0))

  val cacheTag = new Array[Int](accessList.size)
  val valid = new Array[Boolean](accessList.size)
  val addr = accessList.map((a : (Int, Int)) => a(0) * lineSize + a(1))
  print("Addresses accessed: ")
  println(addr)
  addr.foreach(printf("0x%04x, ", _))
  println()

  val mask = (1 << indexBits) - 1
  for a <- addr do
    val idx = (a >> (wordOffsetBits + byteOffsetBits)) & mask
    val tag = (a >> (indexBits + wordOffsetBits + byteOffsetBits))
    val hit = valid(idx) && tag == cacheTag(idx)
    valid(idx) = true
    cacheTag(idx) = tag
    println(s"$tag $idx $hit")

}
