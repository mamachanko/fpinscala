import org.scalatest._

import scala.annotation.tailrec

class NthFibonacciSpec extends FlatSpec with Matchers {

  def fibonacci(n: Int): Int = {
    @tailrec
    def fib(n: Int, a: Int = 0, b: Int = 1, counter: Int = 0): Int =
      if (n == counter) a
      else fib(n, b, a + b, counter + 1)

    fib(n)
  }

  "First Fibonacci" should "be 0" in {
    fibonacci(0) shouldEqual 0
  }

  "Second Fibonacci" should "be 1" in {
    fibonacci(1) shouldEqual 1
  }

  "Third Fibonacci" should "be 1" in {
    fibonacci(2) shouldEqual 1
  }

  "Fourth Fibonacci" should "be 2" in {
    fibonacci(3) shouldEqual 2
  }

  "Fifth Fibonacci" should "be 2" in {
    fibonacci(4) shouldEqual 3
  }

  "Tenth Fibonacci" should "be 34" in {
    fibonacci(9) shouldEqual 34
  }

  "Thirtieth Fibonacci" should "be 514229" in {
    fibonacci(29) shouldEqual 514229
  }
}
