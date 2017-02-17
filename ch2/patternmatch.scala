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

    def drop[A](n: Int, as: List[A]): List[A] = as match {
      case Nil => Nil
      case Cons(x, xs) => {
        if (n == 0) Cons(x, xs)
        else drop(n-1, xs)
      }
    }

    def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
      case Nil => Nil
      case Cons(x, xs) => {
        if (f(x)) dropWhile(xs, f)
        else Cons(x, xs)
      }
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
    println(List.drop(3, List('a', 'b', 'c', 'd', 'e')))
    println(List.drop(5, List('a', 'b', 'c', 'd', 'e')))
    println(List.drop(5, Nil))
    println(List.dropWhile(List(1,2,3,4,5,6,7), (n: Int) => n <= 5))
  }
}
