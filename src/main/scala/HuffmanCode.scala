import scala.collection.mutable

object HuffmanCode {

  type HuffmanType = List[(String, String)]
  type FrequencyType = (String, Int)

  def order(x: FrequencyType): Int = -x._2

  def huffman(freqs: List[FrequencyType]): HuffmanType = {
    if (freqs.length == 1)
      List((freqs.head._1, "0"))
    else {
      val priorityQueue = new mutable.PriorityQueue[FrequencyType]()(Ordering.by(order))
      val map: mutable.Map[String, String] = mutable.Map[String, String]()
      // enqueue frequency into priorityQueue
      freqs.foreach(priorityQueue.enqueue(_))
      // add characters to map
      freqs.foreach(x => map ++= List((x._1, "")))

      // build Huffman code
      while (priorityQueue.length > 1) {
        val e1 = priorityQueue.dequeue
        val e2 = priorityQueue.dequeue
        e1._1.foreach(c => map ++= List((c.toString, "0" + map(c.toString))))
        e2._1.foreach(c => map ++= List((c.toString, "1" + map(c.toString))))
        priorityQueue.enqueue((e1._1 + e2._1, e1._2 + e2._2))
      }
      map.toList.sortBy(_._1)
    }
  }

  def encode(plainText: String, huffmanCode: HuffmanType): String = {
    val map = huffmanCode.toMap
    plainText map { x => map(x.toString) } mkString ""
  }

  def decode(cipherText: String, huffmanCode: HuffmanType): String = {
    val inverseCode = (huffmanCode map {case (x, y) => (y, x)}).toMap

    def helper(cipherText: String, i: Int, tempStr: String, res: String): String = {
      if (cipherText.isEmpty) res
      else {
        if (inverseCode.contains(tempStr)) {
          helper(cipherText.slice(i + 1, cipherText.length), -1, "", res + inverseCode(tempStr))
        } else {
          helper(cipherText, i + 1, tempStr + cipherText.charAt(i + 1).toString, res)
        }
      }
    }

    helper(cipherText, -1, "", "")
  }
}
