object PatternMatch{

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }

    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def tail[A](as: List[A]): List[A] = as match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }

    def setHead[A](head: A, as: List[A]): List[A] = as match {
      case Nil => List(head)
      case Cons(x, xs) => Cons(head, xs)
    }
  }

  def main(args: Array[String]): Unit = {
    val x = this.List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }

    println(x)
    println(List.tail(List(1,2,3,4,5)))
    println(List.setHead(12.24, Nil))
    println(List.setHead('x', List('a', 'b', 'c', 'd')))
  }
}
