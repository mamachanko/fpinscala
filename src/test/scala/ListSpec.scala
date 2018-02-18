import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

class ListSpec extends FlatSpec with Matchers {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def tail[A](xs: List[A]): List[A] = xs match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }

    def drop[A](xs: List[A], n: Int): List[A] =
      if (n == 0) xs
      else List.drop(List.tail(xs), n - 1)

    def dropWhile[A](xs: List[A], predicate: A => Boolean): List[A] = xs match {
      case Nil => Nil
      case Cons(x, tail) =>
        if (predicate(x)) List.dropWhile(tail, predicate)
        else xs
    }

    def init[A](xs: List[A]): List[A] = xs match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, tail) => Cons(x, init(tail))
    }

    def setHead[A](xs: List[A], newHead: A): List[A] = xs match {
      case Nil => List(newHead)
      case Cons(_, tail) => Cons(newHead, tail)
    }

    def foldRight[A, B](xs: List[A], z: B)(f: (A, B) => B): B = xs match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

    @tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def sum2(xs: List[Int]): Int = foldRight(xs, 0)(_ + _)

    def sum3(xs: List[Int]): Int = foldLeft(xs, 0)(_ + _)

    def product2(xs: List[Double]): Double = foldRight(xs, 1.0)(_ * _)

    def product3(xs: List[Double]): Double = foldLeft(xs, 1.0)(_ * _)

    def length[A](xs: List[A]): Int = foldRight(xs, 0)((_, b) => b + 1)

    def length2[A](xs: List[A]): Int = foldLeft(xs, 0)((a, _) => a + 1)

    def reverse[A](xs: List[A]): List[A] = foldLeft(xs, List[A]())((acc, h) => Cons(h, acc))

    def append[A](as: List[A], bs: List[A]): List[A] = foldRight(as, bs)((a, b) => Cons(a, b))

    def concatenate[A](ls: List[A]*): List[A] = foldRight(List(ls: _*), List[A]())(append)

    def sum(is: List[Int]): Int = is match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def plusOne(xs: List[Int]): List[Int] = foldRight(xs, List[Int]())((a, b) => Cons(a + 1, b))

    def doubleToString(xs: List[Double]): List[String] = foldRight(xs, List[String]())((a, b) => Cons(a.toString, b))

    def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, List[B]())((a, b) => Cons(f(a), b))

    def filter[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(x => if (f(x)) List(x) else List())

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, List[B]())((a, b) => concatenate(f(a), b))

    def zipWith[A, B](as: List[A], bs: List[A])(f: (A, A) => B): List[B] = (as, bs) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
      case _ => Nil
    }

    def startsWith[A](xs: List[A], heads: List[A]): Boolean = length(filter(zipWith(xs, heads)(_ == _))(_ == true)) == length(heads)

    @tailrec
    def hasSubsequence[A](xs: List[A], sub: List[A]): Boolean = xs match {
      case Nil => false
      case Cons(_, t) if startsWith(xs, sub) => true
      case Cons(_, t) => hasSubsequence(t, sub)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  def even(a: Int): Boolean = a % 2 == 0

  "List()" should "return Nil" in {
    List() shouldEqual Nil
  }

  "List(1)" should "return Cons(1, Nil)" in {
    List(1) shouldEqual Cons(1, Nil)
  }

  "List(3, 5)" should "return Cons(3, Cons(5, Nil))" in {
    List(3, 5) shouldEqual Cons(3, Cons(5, Nil))
  }

  "List.tail(List()" should "return Nil" in {
    List.tail(List()) shouldEqual Nil
  }

  "List.tail(List(1)" should "return Nil" in {
    List.tail(List()) shouldEqual Nil
  }

  "List.tail(List(1, 2))" should "return Cons(2, Nil)" in {
    List.tail(List(1, 2)) shouldEqual List(2)
  }

  "List.setHead(List(), 1)" should "return List(1)" in {
    List.setHead(List(), 1) shouldEqual List(1)
  }

  "List.setHead(List(1, 2), 3)" should "return List(3, 2)" in {
    List.setHead(List(1, 2), 3) shouldEqual List(3, 2)
  }

  "List.drop(List(), 10)" should "return List()" in {
    List.drop(List(), 10) shouldEqual List()
  }

  "List.drop(List(1), 1)" should "return List()" in {
    List.drop(List(1), 1) shouldEqual List()
  }

  "List.drop(List(1, 2), 1)" should "return List(2)" in {
    List.drop(List(1, 2), 1) shouldEqual List(2)
  }

  "List.drop(List(1, 2), 2)" should "return List()" in {
    List.drop(List(1, 2), 2) shouldEqual List()
  }

  "List.dropWhile(List(), predicate)" should "return List()" in {
    List.dropWhile(List(), even) shouldEqual List()
  }

  "List.dropWhile(List(2, 4, 3), even)" should "return List(3)" in {
    List.dropWhile(List(2, 4, 3), even) shouldEqual List(3)
  }

  "List.init(List())" should "return List()" in {
    List.init(List()) shouldEqual List()
  }

  "List.init(List(1,2,3,4))" should "return List(1,2,3)" in {
    List.init(List(1, 2, 3, 4)) shouldEqual List(1, 2, 3)
  }

  "List.sum2(List(1,2,3,4))" should "return 10" in {
    List.sum2(List(1, 2, 3, 4)) shouldEqual 10
    List.sum3(List(1, 2, 3, 4)) shouldEqual 10
  }

  "List.sum2(List(1,0,2,3,4))" should "return 10" in {
    List.sum2(List(1, 0, 2, 3, 4)) shouldEqual 10
    List.sum3(List(1, 0, 2, 3, 4)) shouldEqual 10
  }

  "List.product2(List(1,2,3,4))" should "return 24" in {
    List.product2(List(1, 2, 3, 4)) shouldEqual 24
    List.product3(List(1, 2, 3, 4)) shouldEqual 24
  }

  "List.product2(List(1,0,2,3,4))" should "return 0" in {
    List.product2(List(1.0, 0.0, 2.0, 3.0, 4.0)) shouldEqual 0.0
    List.product3(List(1.0, 0.0, 2.0, 3.0, 4.0)) shouldEqual 0.0
  }

  "List.length(List())" should "return 0" in {
    List.length(List()) shouldEqual 0
    List.length2(List()) shouldEqual 0
  }

  "List.length(List(2))" should "return 1" in {
    List.length(List(2)) shouldEqual 1
    List.length2(List(2)) shouldEqual 1
  }

  "List.length(List(2,4,8))" should "return 3" in {
    List.length(List(2, 4, 8)) shouldEqual 3
    List.length2(List(2, 4, 8)) shouldEqual 3
  }

  "List.reverse(List())" should "return List()" in {
    List.reverse(List()) shouldEqual List()
  }

  "List.reverse(List(1,2,3))" should "return List(3,2,1)" in {
    List.reverse(List(1, 2, 3)) shouldEqual List(3, 2, 1)
  }

  "List.append(List(), List())" should "return List()" in {
    List.append(List(), List()) shouldEqual List()
  }

  "List.append(List(1), List())" should "return List(1)" in {
    List.append(List(1), List()) shouldEqual List(1)
  }

  "List.append(List(1,2,3), List(4,5,6))" should "return List(1,2,3,4,5,6)" in {
    List.append(List(1, 2, 3), List(4, 5, 6)) shouldEqual List(1, 2, 3, 4, 5, 6)
  }

  "List.concatenate(List(1,2), List(3,4), List(5,6))" should "return List(1,2,3,4,5,6)" in {
    List.concatenate(List(1, 2), List(3, 4), List(5, 6)) shouldEqual List(1, 2, 3, 4, 5, 6)
  }

  "List.plusOne(List())" should "return List()" in {
    List.plusOne(List()) shouldEqual List()
  }

  "List.plusOne(List(3,-1,5))" should "return List(4,0,6)" in {
    List.plusOne(List(3, -1, 5)) shouldEqual List(4, 0, 6)
  }

  "List.doubleToString(List())" should "return List()" in {
    List.doubleToString(List()) shouldEqual List()
  }

  "List.doubleToString(List(3.0,-1.0,5.0))" should "return List(\"3.0\", \"-1.0\", \"5.0\")" in {
    List.doubleToString(List(3.0, -1.0, 5.0)) shouldEqual List("3.0", "-1.0", "5.0")
  }

  "List.map(List())((_ * 2).toString)" should "return List()" in {
    List.map(List[Double]())(a => (a * 2).toString) shouldEqual List()
  }

  "List.map(List(3.0,-1.0,5.0))((_ * 2).toString)" should "return List(\"6.0\", \"-2.0\", \"10.0\")" in {
    List.map(List(3.0, -1.0, 5.0))(a => (a * 2).toString) shouldEqual List("6.0", "-2.0", "10.0")
  }

  "List.filter(List(1,2,3,4,5,6))(_ % 2 == 0)" should "return List(2,4,6)" in {
    List.filter(List(1,2,3,4,5,6))(_ % 2 == 0) shouldEqual List(2,4,6)
  }

  "List.flatMap(List(1,2,3))(i => List(i, i))" should "return List(1,1,2,2,3,3)" in {
    List.flatMap(List(1,2,3))(i => List(i, i)) shouldEqual List(1,1,2,2,3,3)
  }

  "List.zipWith(List(1,2,3), List(4,5,6))(_ + _)" should "return List(5,7,9)" in {
    List.zipWith(List(1,2,3), List(4,5,6))(_ + _) shouldEqual List(5,7,9)
  }

  "List.zipWith(List(1,2), List(4,5,6))(_ + _)" should "return List(5,7)" in {
    List.zipWith(List(1,2), List(4,5,6))(_ + _) shouldEqual List(5,7)
  }

  "List.zipWith(List(1,2,3), List(4,5))(_ + _)" should "return List(5,7)" in {
    List.zipWith(List(1,2,3), List(4,5))(_ + _) shouldEqual List(5,7)
  }

  "List.zipWith(List(1,2,3), List())(_ + _)" should "return List()" in {
    List.zipWith(List(1,2,3), List())(_ + _) shouldEqual List()
  }

  "List.hasSubsequence(List(1,2,3,4), List())" should "return true" in {
    List.hasSubsequence(List(1,2,3,4), List()) shouldBe true
  }

  "List.hasSubsequence(List(1,2,3,4), List(1,2))" should "return true" in {
    List.hasSubsequence(List(1,2,3,4), List(1,2)) shouldBe true
  }

  "List.hasSubsequence(List(1,2,3,4), List(2,3))" should "return true" in {
    List.hasSubsequence(List(1,2,3,4), List(2,3)) shouldBe true
  }

  "List.hasSubsequence(List(1,2,3,4), List(4))" should "return true" in {
    List.hasSubsequence(List(1,2,3,4), List(4)) shouldBe true
  }

  "List.hasSubsequence(List(1,2,3,4), List(1,2,3,4))" should "return true" in {
    List.hasSubsequence(List(1,2,3,4), List(1,2,3,4)) shouldBe true
  }

  "List.hasSubsequence(List(1,2,3,4), List(7))" should "return false" in {
    List.hasSubsequence(List(1,2,3,4), List(1,7,3)) shouldBe false
  }

  "List.hasSubsequence(List(1,2,3,4), List(1,3))" should "return false" in {
    List.hasSubsequence(List(1,2,3,4), List(1,3)) shouldBe false
  }

}
