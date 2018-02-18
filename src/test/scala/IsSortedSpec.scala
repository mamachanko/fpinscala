import org.scalatest._

class IsSortedSpec extends FlatSpec with Matchers {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = as match {
    case Array() => true
    case Array(_) => true
    case Array(first, second, _*) =>
      if (ordered(first, second)) isSorted(as.tail, ordered)
      else false
  }

  "[]" should "be sorted" in {
    isSorted[Int](Array(), (a, b) => a <= b) shouldBe true
  }

  "[1]" should "be sorted" in {
    isSorted[Int](Array(1), (a, b) => a <= b) shouldBe true
  }

  "[1, 2]" should "be sorted" in {
    isSorted[Int](Array(1, 2), (a, b) => a <= b) shouldBe true
  }

  "[2, 1]" should "not be sorted" in {
    isSorted[Int](Array(2, 1), (a, b) => a <= b) shouldBe false
  }

  "[\"a\", \"b\", \"c\"]" should "be sorted" in {
    isSorted[String](Array("a", "b", "c"), (a, b) => a <= b) shouldBe true
  }

  "[\"b\", \"c\", \"a\"]" should "not be sorted" in {
    isSorted[String](Array("b", "c", "a"), (a, b) => a <= b) shouldBe false
  }

  "[[], [3, 2], [1, 1, 4]]" should "be sorted" in {
    isSorted[Array[Int]](Array(Array(), Array(3, 2), Array(1, 1, 4)), (a, b) => a.length <= b.length) shouldBe true
  }

}
