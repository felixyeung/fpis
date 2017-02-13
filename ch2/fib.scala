object MyProg {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n0: Int, n1: Int, n: Int): Int = {
      if (n == 0) n1
      else go(n1, n0 + n1, n-1)
    }

    go(0, 1, n)
  }

  def main(args: Array[String]): Unit = {
    println(fib(1))
    println(fib(2))
    println(fib(3))
    println(fib(4))
    println(fib(5))
    println(fib(6))
    println(fib(7))
    println(fib(8))
    println(fib(9))
    println(fib(10))
    println(fib(20))
  }
}
