import org.specs2.Specification
import Lists._
class ListsSpec extends Specification { def is =
  s2"""
      verify all problems related to lists
      verify the last element problem pb1
      verify the lenght of a list pb4
     verify reversed list pb5
     verify palindrome pb6
      verify flatten list pb7
      | verify distinct list pb8
      |verify packaging pb9
      |verify lotto $pb23

    """

  lazy val testList = List(10,12,255,0,1000)
  lazy val listToRevers = List("a", "b", "c")
  lazy val expectedListToRevers = List("c", "b", "a")
  lazy val palindromeList = List(1,54,2,54,1)
  lazy val notPalindromeList = List(1,3,3,5)
  lazy val listToFlatten = List(List(1,2,5), List(12,10,List(5)),3)
  lazy val flattenList = List(1,2,5,12,10,5,3)
  lazy val listConsc = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  lazy val expectedEliminatedDupli = List('a, 'b, 'c, 'a, 'd, 'e)
  lazy val expectedConscList = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

//
//  def pb1 = lastRecur(testList) must beEqualTo(1000)
//
//  def pb4 = listLength(testList) must beEqualTo(5)
//
//  def pb5 = reverse(listToRevers) must beEqualTo(expectedListToRevers)
//
//  def pb6 =  (isPalindrome(palindromeList) mustEqual(true)) and (isPalindrome(notPalindromeList) mustEqual(false))
//
//  def pb7 = flatten(listToFlatten) must beEqualTo(flattenList)
//
//  def pb8 = removeConsec(listConsc) must beEqualTo(expectedEliminatedDupli)
//
//  def pb9 = pack(listConsc) must beEqualTo(expectedConscList)


  def pb23 = randomSelect(3, (1 to 10).toList) must Have
  def pb24 = lotto(6, 49) must beEqualTo(List(23, 1, 17, 33, 21, 37))

}
