package ru.sergeif

object Shape{
  def apply(a: Any): List[Int] = a match {
    case s: List[_] => s.length :: this.apply(s.head)
    case _ => Nil
  }
}

object CommonFrame{
  def apply(l: List[Int], r: List[Int]): Option[List[Int]] = (l, r) match {
    case (Nil, Nil) => Some(Nil)
    case (x::xs, Nil) => Some(Nil)
    case (Nil, y::ys) => Some(Nil)
    case (x::xs,y::ys) => if (x == y) for (t <- CommonFrame(xs, ys)) yield x::t else None
    case _ => None
  }
}

abstract class JToken
case class JNoun(v: Any, shape: List[Int]) extends JToken
case class JVerb(s: String) extends JToken

class JMonad(s: String, r: Int, mApply: Any => Any) extends JVerb(s){
  def apply(a: Any): Any =  {
    val shape = Shape(a)
    if (shape.length <= r) mApply(a) else a.asInstanceOf[List[_]].map(apply)
  }
}
class JDyad(s: String, rank: (Int,Int), dApply: (Any,Any) => Any) extends JVerb(s){
  def apply(x: Any, y: Any): Any = {
    val leftShape = Shape(x)
    val rightShape = Shape(y)
    val leftRank = rank._1
    val rightRank = rank._2
    val leftFrame = if (leftRank != Int.MaxValue) leftShape.take(leftShape.length - leftRank) else leftShape
    val rightFrame = if (rightRank != Int.MaxValue) rightShape.take(rightShape.length - rightRank) else rightShape
    val commonFrameOpt = CommonFrame(leftFrame, rightFrame)
    if (commonFrameOpt == None)
      throw new RuntimeException(s"length error in dyad $s")
    if (commonFrameOpt.getOrElse(Nil) != Nil){
      val leftArg = x.asInstanceOf[List[Any]]
      val rightArg = y.asInstanceOf[List[Any]]
      for {
        (left,right) <- leftArg zip rightArg
      } yield apply(left, right)
    }else{
      if (leftFrame.length == rightFrame.length && rightFrame.length == 0)
        dApply(x, y)
      else if (leftFrame.length > rightFrame.length)
        x.asInstanceOf[List[Any]].map(apply(_,y))
      else
        y.asInstanceOf[List[Any]].map(apply(x,_))
    }
  }
}



object ScalaJConsole extends App {
  def increment(a: Any) = a match {
    case x: Int => x + 1
    case x: BigInt => x + BigInt(1)
    case _ => None
  }
  def reverse(a: Any) = a match  {
    case x: List[_] => x.asInstanceOf[List[_]].reverse
    case _ => None
  }
  def plus(x: Any, y: Any) = (x,y) match {
    case (a: Int, b: Int) => a + b
    case (a: Int, b: BigInt) => a + b
    case (a: BigInt, b: Int) => a + b
    case (a: BigInt, b: BigInt) => a + b
    case _ => None
  }
  val incr = new JMonad(">:", 0, increment)
  val inver = new JMonad("|.", Int.MaxValue, reverse)
  val inver1 = new JMonad("|.\"1", 1, reverse)
  val pluss = new JDyad("+", (0,0), plus)
  println(incr(3))
  println(inver(List(List(5,6))))
  println(inver1(List(List(5,6), List(1,2,3))))
  println(pluss(1,2))
  println(pluss(List(1,2,3),2))
  //println(pluss(List(1,2,3),List(2,3)))
  println(pluss(List(1,2,3),List(-1,-1,-1)))
  println(pluss(List(List(1,2,3), List(3,4,5)),List(List(-1,-1,-1),List(-5,-5,-5))))
}