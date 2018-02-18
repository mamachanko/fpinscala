import org.scalatest._

class TreeSpec extends FlatSpec with Matchers {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def size[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

    def depth[A](t: Tree[A]): Int = fold(t)(_ => 1)((dl, dr) => 1 + (dl max dr))

    def maximum(t: Tree[Int]): Int = fold(t)(x => x)(_ max _)

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_, _))

    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

  "size" should "return 1" in {
    Tree.size(Leaf(1)) shouldEqual 1
  }

  "size" should "return 3" in {
    Tree.size(Branch(
      Leaf(1),
      Leaf(2)
    )) shouldEqual 3
  }

  "size" should "return 5" in {
    Tree.size(Branch(
      Leaf(1),
      Branch(
        Leaf(2),
        Leaf(3)
      )
    )) shouldEqual 5
  }

  "size" should "return 7" in {
    Tree.size(Branch(
      Branch(
        Leaf(1),
        Leaf(2)
      ),
      Branch(
        Leaf(3),
        Leaf(4)
      )
    )) shouldEqual 7
  }

  "maximum" should "return 1" in {
    Tree.maximum(Leaf(1)) shouldEqual 1
  }

  "maximum" should "return 3" in {
    Tree.maximum(Branch(
      Leaf(1),
      Branch(
        Leaf(2),
        Leaf(3)
      )
    )) shouldEqual 3
  }

  "maximum" should "return 4" in {
    Tree.maximum(Branch(
      Branch(
        Leaf(1),
        Leaf(2)
      ),
      Branch(
        Leaf(4),
        Leaf(3)
      )
    )) shouldEqual 4
  }

  "depth" should "return 1" in {
    Tree.depth(Leaf(1)) shouldEqual 1
  }

  "depth" should "return 2" in {
    Tree.depth(Branch(
      Leaf(1),
      Leaf(2)
    )) shouldEqual 2
  }

  "depth" should "return 3" in {
    Tree.depth(Branch(
      Leaf(1),
      Branch(
        Leaf(2),
        Leaf(3)
      )
    )) shouldEqual 3
  }

  "depth" should "also return 3" in {
    Tree.depth(Branch(
      Branch(
        Leaf(1),
        Leaf(2)
      ),
      Branch(
        Leaf(4),
        Leaf(3)
      )
    )) shouldEqual 3
  }

  "depth" should "return 4" in {
    Tree.depth(Branch(
      Branch(
        Leaf(1),
        Leaf(2)
      ),
      Branch(
        Branch(
          Leaf(4),
          Leaf(3)
        ),
        Leaf(5)
      )
    )) shouldEqual 4
  }

  "map" should "map" in {
    Tree.map(Branch(
      Branch(
        Leaf(1),
        Leaf(2)
      ),
      Branch(
        Branch(
          Leaf(4),
          Leaf(3)
        ),
        Leaf(5)
      )
    ))(_ + 1) shouldEqual Branch(
      Branch(
        Leaf(2),
        Leaf(3)
      ),
      Branch(
        Branch(
          Leaf(5),
          Leaf(4)
        ),
        Leaf(6)
      )
    )
  }

}
