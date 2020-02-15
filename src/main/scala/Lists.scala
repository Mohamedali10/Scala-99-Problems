import scala.util.Random

object Lists {

  def lastElement[A](list: List[A]): A = list.last
  //recursively
  def lastRecur[A](list: List[A]): A = list match {
    case _ :: xf :: Nil => xf
    case _ :: xs  => lastRecur(xs)
    case _        => throw new NoSuchElementException
  }

  def beforeLast[A](list: List[A]): A  = list.init.last
  def getElement[A](list: List[A], index: Int):A =  list.drop(index).head

  def listLength[A](list: List[A]) : Int = list match {
    case _ :: Nil => 1
    case _ :: xs  => listLength(xs) + 1
    case Nil => 0
  }

  def reverseList[A](list: List[A]): List[A] = list.reverse
   // recursively
  def reverse[A](list: List[A]): List[A] = {
    if (list.nonEmpty)
    {
      list match {
        case x :: xs  => {
          println(x)
          reverse(xs) ++ List(x)
        }
        case x :: Nil => List(x)
      }
    }
    else Nil
  }

  def isPalindrome[A](list:List[A]) : Boolean = {
    if(list.isEmpty) false
    else
      {
        (list corresponds list.reverse)(_ == _)
      }
  }

  def flatten(list: List[Any]): List[Any] = {
    list flatMap  {
      case ls : List[_] => flatten(ls)
      case elem => List(elem)
    }
  }

  def removeConsec[A](list: List[A], isSet: Boolean = false): List[A] = list match {
    case x :: Nil => List(x)
    case x::xs if(x == xs.head) => {
      if(isSet) removeConsec(xs, true)
      else removeConsec(xs, true) }
    case x::xs if x != xs.head => x +: removeConsec(xs)
  }

  def pack[A](list: List[A]): List[List[A]] = {
    list match {
      case Nil => Nil
      case x :: xs =>
        val subList = xs span(ele => ele == x)
        x +: subList._1 :: pack(subList._2)
    }
  }

  //20
  def removeAt[A](index: Int, list: List[A]) = {
    if(list.isEmpty) Nil
    else list.take(index) ::: list.drop(index+1)
  }

  def randomSelect(len: Int, list:List[Int]) = {
    val rnd = new Random(len).nextInt()
    for(i <- 1 to len) yield {
      removeAt(i, list)
    }
  }

  def lotto(l:Int, m:Int) = {
    val li = (1 to m).toList
    randomSelect(l, li)
  }



   //removeAt(3,p)
}
