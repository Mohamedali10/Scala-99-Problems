import scala.util.Random

object Lists {

  //P01
  def lastElement[A](list: List[A]): A = list.last
  //recursively
  def lastRecur[A](list: List[A]): A = list match {
    case _ :: xf :: Nil => xf
    case _ :: xs  => lastRecur(xs)
    case _        => throw new NoSuchElementException
  }

  //P02
  def beforeLast[A](list: List[A]): A  = list.init.last

  //P03
  def getElement[A](list: List[A], index: Int):A =  list.drop(index).head

  //P04
  def listLength[A](list: List[A]) : Int = list match {
    case _ :: Nil => 1
    case _ :: xs  => listLength(xs) + 1
    case Nil => 0
  }

  //P05
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

  //P06
  def isPalindrome[A](list:List[A]) : Boolean = {
    if(list.isEmpty) false
    else
      {
        (list corresponds list.reverse)(_ == _)
      }
  }

  //P07
  def flatten(list: List[Any]): List[Any] = {
    list flatMap  {
      case ls : List[_] => flatten(ls)
      case elem => List(elem)
    }
  }

  //P08
  def removeConsec[A](list: List[A], isSet: Boolean = false): List[A] = list match {
    case x :: Nil => List(x)
    case x::xs if(x == xs.head) => {
      if(isSet) removeConsec(xs, true)
      else removeConsec(xs, true) }
    case x::xs if x != xs.head => x +: removeConsec(xs)
  }

  //P09
  def pack[A](list: List[A]): List[List[A]] = {
    list match {
      case Nil => Nil
      case x :: xs =>
        val subList = xs span(ele => ele == x)
        x +: subList._1 :: pack(subList._2)
    }
  }

  //P10
  def encoding[A](list: List[A]) : List[(Int,A)]=
  {
    pack(list).map(ele=> (ele.size, ele.head))
  }
  //P11
  def encodeModified[A](list: List[A]) =
  pack(list).map { ele =>
    if(ele.size > 1) (ele.size, ele.head)
    else ele.head
  }
  //P12
  def decode[A](list: List[A]) =
    encoding(list).flatMap { list =>
    //for( i <- 1 to list._1) yield list._2
    List.fill(list._1)(list._2)
  }
  //P13
  def pack2[A](li: List[A]): List[(Int,A)] = {
    if(li.isEmpty) Nil
    else {
      val (packed, next) = li.span(_ == li.head)
      (packed.size, packed.head) ::pack2(next)
    }
  }

  //P14 && P15
  def duplicatList[A](list: List[A], times: Int) : List[A] = {
    list.flatMap{List.fill(times)(_)}
  }

  //P16
  def drop2[A](list: List[A], index: Int) : List[A] = {
    if (list.isEmpty) {
      Nil
    } else {
      val (updated, toUpdate) = list.splitAt(index)
      updated.init ::: drop2(toUpdate,index)
    }
  }

  //P17
  def split[A](len: Int, list: List[A]) = { // (list.take(len), list.drop(len)) //list.splitAt(len)

    def splitRec[A](acc: List[A], res:List[A], len: Int): (List[A], List[A]) = {
      if(res.isEmpty) (acc,res)
      else if (len == 0) (acc, res)
      else splitRec(acc :+ res.head , res.tail, len - 1 )
    }

    splitRec(List(), list, len)
  }

  //P18
  def slice[A](inf: Int, sup: Int, list: List[A]) = {
    //list.slice(inf, sup)
    //list.drop(inf).dropRight(sup)
    def sliceRec[A](acc:List[A], list2: List[A], inf1: Int):List[A] = {
      if(list2.isEmpty) acc
      else if (inf1 >= inf && inf1 < sup) sliceRec(acc :+ list2.head, list2.tail, inf1 + 1)
      else sliceRec(acc, list2.tail,  inf1 + 1)
    }
    sliceRec(List(), list, 0)
  }

  //P19
  def rotateLeft[A](rot: Int, list: List[A]) = {
    if (list.isEmpty) Nil
    else   list.drop(rot) ::: list.take(rot)
  }

  //P20
  def removeAt[A](index: Int, list: List[A]) = {
    if(list.isEmpty) Nil
    else list.take(index) ::: list.drop(index+1)
  }

  //P21
  def insertAt[A](index: Int, list: List[A], ele: A) =
    list.take(index-1) ::: List(ele) ::: list.drop(index-1)

  //P22
  def range(a: Int, b: Int) ={
    (for(i<- a to b) yield List(i)).toList.flatten
  }

  //P23
  def randomSelect(len: Int, list:List[Int]) = {
    val rnd = new Random(len).nextInt()
    for(i <- 1 to len) yield {
      removeAt(i, list)
    }
  }

  //P24
  def lotto(l:Int, m:Int) = {
    val li = (1 to m).toList
    randomSelect(l, li)
  }
}
