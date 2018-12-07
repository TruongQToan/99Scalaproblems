import org.scalatest.FunSuite

class HuffmanCodeTest extends FunSuite {
  import HuffmanCode._
  test("Test huffman function with empty list") {
    assert(huffman(List()) == List())
  }

  test("Test huffman function with one character vocabulary") {
    assert(huffman(List(("a", 1))) == List(("a", "0")))
  }

  test("Test huffman function with vocabulary that has many characters") {
    val freqs = List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))
    val huffmanCode = List(("a", "0"), ("b", "101"), ("c", "100"), ("d", "111"), ("e", "1101"), ("f", "1100"))
    assert(huffman(freqs) == huffmanCode)
  }

  test("Test encode function with empty string") {
    val huffmanCode = List(("a", "0"), ("b", "101"), ("c", "100"), ("d", "111"), ("e", "1101"), ("f", "1100"))
    assert(encode("", huffmanCode) == "")
  }

  test("Test encode function with empty huffman code") {
    assert(encode("abe", List()) == "abe")
  }

  test("Test encode function with non-empty string and non-empty code ") {
    val huffmanCode = List(("a", "0"), ("b", "101"), ("c", "100"), ("d", "111"), ("e", "1101"), ("f", "1100"))
    assert(encode("abe", huffmanCode) == "01011101")
  }

  test("Test decode function with empty cipher text") {
    val huffmanCode = List(("a", "0"), ("b", "101"), ("c", "100"), ("d", "111"), ("e", "1101"), ("f", "1100"))
    assert(decode("", huffmanCode) == "")
  }

  test("Test decode function with empty huffman code") {
    assert(decode("abe", List()) == "abe")
  }

  test("Test decode function with given cipher text and huffman code") {
    val huffmanCode = List(("a", "0"), ("b", "101"), ("c", "100"), ("d", "111"), ("e", "1101"), ("f", "1100"))
    assert(decode("01011101", huffmanCode) == "abe")
  }

  test("Test encode and decode function give consistent result") {
    val huffmanCode = List(("a", "0"), ("b", "101"), ("c", "100"), ("d", "111"), ("e", "1101"), ("f", "1100"))
    val cipherText = encode("abe", huffmanCode)
    assert(decode(cipherText, huffmanCode) == "abe")
  }
}
