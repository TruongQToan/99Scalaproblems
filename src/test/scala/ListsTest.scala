import org.scalatest.FunSuite

class ListsTest extends FunSuite {
  import Lists._


  // Last element
  test("Last element of empty list") {
    assert(lastElement(List()).isEmpty)
  }

  test("Last element of list which has one element") {
    assert(lastElement(List(1)).get == 1)
  }

  test("Last element of which has more than one elements") {
    assert(lastElement(List(1, 2, 3)).get == 3)
  }


  // Last but one element
  test("Last but one element of empty list") {
    assert(lastButOneElement(List()).isEmpty)
  }

  test("Last but one element of list which has one element") {
    assert(lastButOneElement(List(1)).isEmpty)
  }

  test("Last but one element of which has more than one elements") {
    assert(lastButOneElement(List(1, 2, 3)).get == 2)
  }


  // kth element
  test("First element of empty list") {
    assert(kthElement(List(), 0).isEmpty)
  }

  test("First element of list which has one element") {
    assert(kthElement(List(1), 0).get == 1)
  }

  test("Second element of list which has one element") {
    assert(kthElement(List(1), 1).isEmpty)
  }

  test("Third element of list which has four elements") {
    assert(kthElement(List(1, 2, 3, 4), 2).get == 3)
  }


  // number of elements
  test("Number of elements of empty list") {
    assert(numberOfElements(List()) == 0)
  }

  test("Number of elements of list which is not empty") {
    assert(numberOfElements(List(1, 3, 4)) == 3)
  }


  // reverse a list
  test("Reverse an empty list") {
    assert(reverseList(List()) == List())
  }

  test("Reverse a list which has one element") {
    assert(reverseList(List(1)) == List(1))
  }

  test("Reverse a list which has more than one elements") {
    assert(reverseList(List(1, 2, 3)) == List(3, 2, 1))
  }


  // find out whether a list is a palindrome
  test("Check whether an empty list is a palindrome") {
    assert(isPalindrome(List()))
  }

  test("Check whether a list with one element is a palindrome") {
    assert(isPalindrome(List(1)))
  }

  test("Check whether List(1, 3, 1) is a palindrome") {
    assert(isPalindrome(List(1, 3, 1)))
  }

  test("Check whether List(1, 2) ia a palindrome") {
    assert(!isPalindrome(List(1, 2)))
  }


  // Flatten a nested list structure
  test("Flatten an empty list") {
    assert(flatten(List()) == List())
  }

  test("Flatten a list with one empty list element") {
    assert(flatten(List(List())) == List())
  }

  test("Flatten a list with two empty list elements") {
    assert(flatten(List(List(), List())) == List())
  }

  test("Flatten List(List(1, 2), List(3, 4), List(5, 6))") {
    assert(flatten(List(List(1, 2), List(3, 4), List(5, 6))) == List(1, 2, 3, 4, 5, 6))
  }


  // eliminate consecutive duplicates of list elements
  test("Eliminate consecutive duplicates of empty list") {
    assert(compress(List()) == List())
  }

  test("Eliminate consecutive duplicates of a list which has one element") {
    assert(compress(List(1)) == List(1))
  }

  test("Eliminate consecutive duplicates of a list which has unique element") {
    assert(compress(List(1, 2, 3)) == List(1, 2, 3))
  }

  test("Eliminate consecutive duplicates of List(1, 1, 2, 2, 3, 3, 4)") {
    assert(compress(List(1, 1, 2, 2, 3, 3, 4)) == List(1, 2, 3, 4))
  }


  // pack consecutive duplicates of list elements into sublists
  test("Pack consecutive elements of empty list") {
    assert(pack(List()).isEmpty)
  }

  test("Pack consecutive elements of a list with one element") {
    assert(pack(List(1)) == List(List(1)))
  }

  test("Pack consecutive elements of a list with unique elements") {
    assert(pack(List(1, 2, 3)) == List(List(1), List(2), List(3)))
  }

  test("Pack consecutive elements of List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4)") {
    assert(pack(List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4)) == List(List(1, 1, 1), List(2, 2, 2), List(3, 3, 3), List(4)))
  }


  // run length encoding of a list
  test("Run length encoding of empty list") {
    assert(encode(List()).isEmpty)
  }

  test("Run length encoding of a list with one element") {
    assert(encode(List(1)) == List((1, 1)))
  }

  test("Run length encoding a list with unique elements") {
    assert(encode(List(1, 2, 3)) == List((1, 1), (1, 2), (1, 3)))
  }

  test("Run length encoding of List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4)") {
    assert(encode(List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4)) == List((3, 1), (3, 2), (3, 3), (1, 4)))
  }

  // duplicate the elements of a list
  test("Duplicate the element of empty list") {
    assert(duplicate(List()).isEmpty)
  }

  test("Duplicate the element of List(1)") {
    assert(duplicate(List(1)) == List(1, 1))
  }

  test("Duplicate the element of List(1, 2, 3)") {
    assert(duplicate(List(1, 2, 3)) == List(1, 1, 2, 2, 3, 3))
  }

  test("Duplicate the element of List(1, 1, 2)") {
    assert(duplicate(List(1, 1, 2)) == List(1, 1, 1, 1, 2, 2))
  }


  // duplicate the elements of a list a given number of times
  test("Duplicate the element of empty list a given number of time") {
    assert(duplicateN(List(), 1).isEmpty)
  }

  test("Duplicate the element of list zero times") {
    assert(duplicateN(List(1), 0) == List())
  }

  test("Duplicate the element of List(1, 2, 3) one times") {
    assert(duplicateN(List(1, 2, 3), 1) == List(1, 2, 3))
  }

  test("Duplicate the element of List(1, 2, 3) two times") {
    assert(duplicateN(List(1, 2, 3), 2) == List(1, 1, 2, 2, 3, 3))
  }

  test("Duplicate the element of List(1, 1, 2) two times") {
    assert(duplicateN(List(1, 1, 2), 2) == List(1, 1, 1, 1, 2, 2))
  }


  // drop every Nth element from a list
  test("Drop every element from an empty list") {
    assert(drop(List(), 1) == List())
  }

  test("Drop every element from a non-empty list") {
    assert(drop(List(1, 2, 3), 1) == List())
  }

  test("Drop every 3th element from List(1, 2, 3, 4, 5, 6, 7, 8, 9)") {
    assert(drop(List(1, 2, 3, 4, 5, 6, 7, 8, 9), 3) == List(1, 2, 4, 5, 7, 8))
  }

  test("Drop every 2nd element from List(1)") {
    assert(drop(List(1), 2) == List(1))
  }
}