import org.scalatest._

class OptionSpec extends FlatSpec with Matchers {

  trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case Some(get) => Some(f(get))
      case None => None
    }

    def getOrElse[B >: A](default: B): B = this match {
      case Some(get) => get
      case None => default
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

    def orElse[B >: A](ob: Option[B]): Option[B] = this map (Some(_)) getOrElse ob

    def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)
  }

  case class Some[A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  def Try[A](a: => A): Option[A] =
    try {
      Some(a)
    } catch {
      case e: Exception => None
    }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap { aa =>
      b map { bb =>
        f(aa, bb)
      }
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap { m =>
      mean(xs map { x =>
        Math.pow(x - m, 2)
      })
    }

  "Some(x).map(f)" should "return Some(f(x))" in {
    Some(4).map(_ * 2) shouldEqual Some(8)
  }

  "None.map(f)" should "return None" in {
    (None: Option[Int]).map(_ * 2) shouldEqual None
  }

  "Some(x).flatMap(_ => Some(y))" should "return Some(y)" in {
    Some(4).flatMap(_ => Some(2)) shouldEqual Some(2)
  }

  "Some(x).flatMap(f => None)" should "return None(x)" in {
    Some(4).flatMap(x => None) shouldEqual None
  }

  "None.flatMap(f)" should "return None" in {
    (None: Option[Int]).flatMap(x => Some(x * 2)) shouldEqual None
  }

  "Some(x).getOrElse(y)" should "return x" in {
    Some(4).getOrElse(8) shouldEqual 4
  }

  "None.getOrElse(y)" should "return y" in {
    None.getOrElse(4) shouldEqual (4)
  }

  "Some(x).orElse(Some(8))" should "return Some(4)" in {
    Some(4).orElse(Some(8)) shouldEqual Some(4)
  }

  "None.orElse(Some(y))" should "return Some(y)" in {
    None.orElse(Some(8)) shouldEqual Some(8)
  }

  "Some(4).filter(even)" should "return Some(4)" in {
    Some(4).filter(_ % 2 == 0) shouldEqual Some(4)
  }

  "Some(4).filter(odd)" should "return None" in {
    Some(4).filter(_ % 2 != 0) shouldEqual None
  }

  "None.filter(Some(y))" should "return None" in {
    (None: Option[Int]).filter(_ % 2 == 0) shouldEqual None
    (None: Option[Int]).filter(_ % 2 != 0) shouldEqual None
  }

  "variance(List(1))" should "return Some(0)" in {
    variance(List(1)) shouldEqual Some(0)
  }

  "variance(List(2,3,1))" should "return Some(2.0/3.0)" in {
    variance(List(2, 3, 1)) shouldEqual Some(2.0 / 3)
  }

  "variance(List())" should "return None" in {
    variance(List()) shouldEqual None
  }

  "map2(Some(1), Some(2))(_ + _)" should "Some(3)" in {
    map2(Some(1), Some(2))(_ + _) shouldEqual Some(3)
  }

  "map2(None, Some(2))(_ + _)" should "None" in {
    map2(None: Option[Int], Some(2))(_ + _) shouldEqual None
    map2(Some(2), None: Option[Int])(_ + _) shouldEqual None
    map2(None: Option[Int], None: Option[Int])(_ + _) shouldEqual None
  }

  "sequence(List(Some(1), Some(2), Some(3)))" should "return Some(List(1,2,3))" in {
    sequence(List(Some(1), Some(2), Some(3))) shouldEqual Some(List(1, 2, 3))
  }

  "sequence(List(Some(1), None, Some(3)))" should "return None" in {
    sequence(List(Some(1), None, Some(3))) shouldEqual None
  }

  "sequence(List())" should "return Some(List())" in {
    sequence(List()) shouldEqual Some(List())
  }

  "traverse(List(\"1\", \"2\", \"3\"))(i => Try(i.toInt))" should "return Some(List(1,2,3))" in {
    traverse(List("1", "2", "3"))(i => Try(i.toInt)) shouldEqual Some(List(1, 2, 3))
  }

  "traverse(List(\"1\", \"x\", \"3\"))(i => Try(i.toInt))" should "return Some(List(1,2,3))" in {
    traverse(List("1", "x", "3"))(i => Try(i.toInt)) shouldEqual None
  }

}
