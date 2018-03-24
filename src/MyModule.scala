object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))

    println(factorial(5))

    println(fib(100))

    println(formatResult("Absolute value ", -42, abs))
    println(formatResult("factorial ", 7, factorial))

    println(formatResult("increment", 7, (x: Int) => x + 1))
    println(formatResult("increment", 7, (x) => x + 1))
    println(formatResult("increment", 7, x => x + 1))
    println(formatResult("increment", 7, x => x + 1))
    println(formatResult("increment", 7, _ + 1))


    var z = Array(1, 2, 3, 4)

    def asc = {
      (x: Int, y: Int) => {
        if (x > y) true
        else false
      }
    }

    def desc = {
      (x: Int, y: Int) => {
        if (x < y) true
        else false
      }
    }


    println(isSorted(z, asc))
    println(isSorted(z, desc))
  }

  def factorial(n: Int): Int = {

    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }

    go(n, 1)
  }


  def fib(n: Int): Long = {
    def myfib(first: Long, second: Long, counter: Int): Long = {
      if (counter == n) {
        second
      } else {
        myfib(second, first + second, counter + 1)
      }
    }

    myfib(0l, 1l, 1)
  }


  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {

    def checkSorted(accumulator: Int): Boolean = {
      if (accumulator >= 1)
        if (gt(as(accumulator), as(accumulator - 1))) {
          checkSorted(accumulator - 1)
        } else {
          false
        }
      else
        true
    }

    checkSorted(as.length - 1)
  }
}