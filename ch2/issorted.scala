object IsSorted {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def go(arr: Array[A], ordered: (A, A) => Boolean, i: Int): Boolean = {
      if (arr.length == 1) true
      else if (i == arr.length - 1) true
      else if (ordered(arr(i-1), arr(i))) go(arr, ordered, i+1)
      else false
    }
    go(as, ordered, 1)
  }
    

  def main(args: Array[String]): Unit = {
    val sortedArr = Array(1, 2, 3, 4, 4, 10)
    val unsortedArr = Array(1, 2, 4, 1, 10)
    val comp = (a: Int, b:Int) => a <= b

    println(isSorted(sortedArr, comp))
    println(isSorted(unsortedArr, comp))
  }
}
