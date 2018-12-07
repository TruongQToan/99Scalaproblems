import org.scalatest.FunSuite

class GrayCodeTest extends FunSuite {
  import GrayCode._

  test("Gray code with n = 0") {
    assert(gray(0) == List())
  }

  test("Gray code with n = 1") {
    assert(gray(1) == List("0", "1"))
  }

  test("Gray code with n = 2") {
    assert(gray(2) == List("00", "01", "11", "10"))
  }

  test("Gray code with n = 3") {
    assert(gray(3) == List("000", "001", "011", "010", "110", "111", "101", "100"))
  }

}
