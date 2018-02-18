import org.scalatest.{FlatSpec, Matchers}

class CurrySpec extends FlatSpec with Matchers {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b: B) => f(a)(b)

  def compose[A, B, C](f: A => B, g: B => C): A => C = a => g(f(a))

  def add(x: Int, y: Int): Int = x + y

  def double(x: Int): Int = x * 2

  "curry" should "curry" in {
    val plusOne = curry(add)(1)
    plusOne(2) shouldEqual 3
  }

  "uncurry" should "uncurry" in {
    val plusCurried = curry(add)
    val plus = uncurry[Int, Int, Int](plusCurried)
    plus(1, 2) shouldEqual 3
  }

  "compose" should "compose" in {
    val plusOne = curry(add)(1)
    val plusOneAndDouble = compose(plusOne, double)
    plusOneAndDouble(1) shouldEqual 4
  }

}
