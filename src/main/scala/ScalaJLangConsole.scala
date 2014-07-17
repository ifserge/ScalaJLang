package ru.sergeif

object Shape{
  def apply(a: Any): List[Int] = a match {
    case s: List[_] => s.length :: this.apply(s.head)
    case _ => Nil
  }
}

class JMonad(r: Int, mApply: Any => Any){
  def apply(a: Any): Any =  {
    val shape = Shape(a)
    if (shape.length <= r) mApply(a) else a.asInstanceOf[List[_]].map(this.apply(_))
  }
}
object CommonFrame{
  def apply(l: List[Int], r: List[Int]): Option[List[Int]] = (l, r) match {
    case (x::xs, Nil) => Some(Nil)
    case (Nil, y::ys) => Some(Nil)
    case (x::xs,y::ys) => if (x == y) for (t <- CommonFrame(xs, ys)) yield x::t else None
    case _ => None
  }
}

class JDyad(r: (Int,Int), dApply: (Any,Any) => Any){
  def apply(x: Any, y: Any): Any = {
    val lshape = Shape(x)
    val rshape = Shape(y)
    val lr = r._1
    val rr = r._2
    val lf = if (lr != Int.MaxValue) lshape.take(lshape.length - lr) else lshape
    val rf = if (rr != Int.MaxValue) rshape.take(rshape.length - rr) else rshape
    val cf =
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
  val incr = new JMonad(0, increment)
  val inver = new JMonad(Int.MaxValue, reverse)
  val inver1 = new JMonad(1, reverse)
  val pluss = new JDyad((0,0), plus)
  println(incr(3))
  println(inver(List(List(5,6))))
  println(inver1(List(List(5,6), List(1,2,3))))
  println(pluss(1,2))
  println(pluss(List(1,2,3),2))
  println(pluss(List(1,2,3),List(2,3)))
  println(pluss(List(1,2,3),List(-1,-1,-1)))
}