object GrayCode {

  import Lists._

  def gray(n: Int): List[String] = {
    def helper(startString: String): List[String] = {
      val n = startString.length
      if (startString.isEmpty) List("")
      else {
        val suffixs = helper(startString.slice(1, n))
        (suffixs map {x => "0" + x}) ::: (reverseList(suffixs) map {x => "1" + x})
      }
    }
    helper("0" * n)
  }

}
