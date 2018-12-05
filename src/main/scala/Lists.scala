import scala.collection.mutable.ListBuffer

object Lists {

  def lastElement[T](xs: List[T]): Option[T] = xs match {
    case Nil => None
    case x :: Nil => Some(x)
    case _ :: ys => lastElement(ys)
  }

  def lastButOneElement[T](xs: List[T]): Option[T] = xs match {
    case Nil => None
    case x :: _ :: Nil => Some(x)
    case _ :: ys => {
      lastButOneElement(ys)
    }
  }

  def kthElement[T](xs: List[T], k: Int): Option[T] = xs match {
    case Nil => None
    case x :: ys =>
      if (k == 0) Some(x)
      else kthElement(ys, k - 1)
  }

  def numberOfElements[T](xs: List[T]): Int = xs match {
    case Nil => 0
    case _ :: ys => 1 + numberOfElements(ys)
  }

  def reverseList[T](xs: List[T]): List[T] = {
    def helper(xs: List[T], buffer: ListBuffer[T]): ListBuffer[T] = xs match {
      case Nil => buffer
      case x :: ys => helper(ys, buffer) += x
    }

    helper(xs, new ListBuffer[T]).toList
  }

  def isPalindrome[T](xs: List[T]): Boolean = reverseList(xs) == xs

  def flatten[T](xs: List[List[T]]): List[T] = xs match {
    case Nil => Nil
    case x :: ys => x ::: flatten(ys)
  }

  def compress[T](xs: List[T]): List[T] = xs match {
    case Nil => Nil
    case x :: y :: ys =>
      if (x == y) compress(y :: ys)
      else x :: compress(y :: ys)
    case _ => xs
  }

  def pack[T](xs: List[T]): List[List[T]] = {
    def helper(xs: List[T], res: List[List[T]],
               lastElement: Option[T]): List[List[T]] = xs match {
      case Nil => res
      case x :: ys =>
        if (lastElement.isEmpty || lastElement.get != x) helper(ys, List(x) :: res, Some(x))
        else helper(ys, (x :: res.head) :: res.tail, Some(x))
    }

    reverseList(helper(xs, List(), None))
  }

  def encode[T](xs: List[T]): List[(Int, T)] = {
    val packList = pack(xs)
    packList.map { x: List[T] => (x.length, x.head) }
  }

  def duplicate[T](xs: List[T]): List[T] = xs match {
    case Nil => Nil
    case x :: ys =>
      List(x, x) ::: duplicate(ys)
  }

  def duplicateN[T](xs: List[T], N: Int): List[T] = {
    def helper(x: T, res: List[T], N: Int): List[T] =
      if (N == 0) res
      else helper(x, x :: res, N - 1)

    flatten(xs.map { x => helper(x, List(), N) })
  }

  def drop[T](xs: List[T], N: Int): List[T] = {
    val oldN = N

    def helper(xs: List[T], N: Int): List[T] =
      xs match {
        case Nil => Nil
        case x :: ys =>
          if (N == 1) helper(ys, oldN)
          else x :: helper(ys, N - 1)
      }

    helper(xs, oldN)
  }

  def split[T](xs: List[T], N: Int): (List[T], List[T]) = {
    def helper(xs: List[T], res: List[T], N: Int): (List[T], List[T]) = xs match {
      case Nil => (reverseList(res), Nil)
      case x :: ys =>
        if (N == 0) (reverseList(res), x :: ys)
        else helper(ys, x :: res, N - 1)
    }
    helper(xs, List(), N)
  }

  def slice[T](xs: List[T], from: Int, end: Int): List[T] = {
    def helper(xs: List[T], res: List[T], k: Int): List[T] = xs match {
      case Nil => reverseList(res)
      case x :: ys =>
        if (k >= from && k < end) helper(ys, x :: res, k + 1)
        else helper(ys, res, k + 1)
    }
    helper(xs, List(), 0)
  }
}