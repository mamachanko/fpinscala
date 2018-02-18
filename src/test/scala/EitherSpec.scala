import org.scalatest._

class EitherSpec extends FlatSpec with Matchers {

  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(r) => Right(f(r))
      case Left(l) => Left(l)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(r) => f(r)
      case Left(l) => Left(l)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(r) => Right(r)
      case Left(_) => b
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        aa <- this
        bb <- b
      } yield f(aa, bb)
  }

  object Either {
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)

    def traverse[E, A, B](l: List[A])(f: A => Either[E, B]): Either[E, List[B]] = l match {
      case Nil => Right(Nil)
      case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
    }
  }

  case class Left[E](value: E) extends Either[E, Nothing]

  case class Right[A](value: A) extends Either[Nothing, A]

  "Right(2).map(x => x * 2)" should "return Right(4)" in {
    Right(2).map(x => x * 2) shouldEqual Right(4)
  }

  "Left(\"error\").map(x => x * 2)" should "return Left(\"error\")" in {
    (Left("error"): Either[String, Int]).map(x => x * 2) shouldEqual Left("error")
  }

  "Right(2).flatMap(x => Right(3))" should "return Right(3)" in {
    Right(2).flatMap(x => Right(3)) shouldEqual Right(3)
  }

  "Right(2).flatMap(x => Left(\"error\"))" should "return Left(\"error\")" in {
    Right(2).flatMap(x => Left("error")) shouldEqual Left("error")
  }

  "Left(\"error\").flatMap(x => Right(3))" should "return Left(\"error\")" in {
    Left("error").flatMap(x => Right(3)) shouldEqual Left("error")
  }

  "Left(\"error\").flatMap(x => Left(\"another error\"))" should "return Left(\"error\")" in {
    Left("error").flatMap(x => Left("another error")) shouldEqual Left("error")
  }

  "Right(1).orElse(Right(2))" should "return Right(1)" in {
    Right(1).orElse(Right(2)) shouldEqual Right(1)
  }

  "Left(\"error\").orElse(Right(2))" should "return Right(2)" in {
    Left("error").orElse(Right(2)) shouldEqual Right(2)
  }

  "Right(1) map2 (Right(2))(_ + _)" should "return Right(3)" in {
    Right(1).map2(Right(2))(_ + _) shouldEqual Right(3)
  }

  "Left(\"error\") map2 (Right(2))(_ + _)" should "return Left(\"error\")" in {
    (Left("error"): Either[String, Int]).map2(Right(2))(_ + _) shouldEqual Left("error")
  }

  "Right(2) map2 (Left(\"error\"))(_ + _)" should "return Left(\"error\")" in {
    Right(2).map2(Left("error"): Either[String, Int])(_ + _) shouldEqual Left("error")
  }

  "sequence(Right(1), Right(2), Right(3))" should "return Right(List(1,2,3))" in {
    Either.sequence(List(Right(1), Right(2), Right(3))) shouldEqual Right(List(1, 2, 3))
  }

  "sequence(Right(1), Left(\"error\"), Right(3), Left(\"another error\"))" should "return Error(\"error\")" in {
    Either.sequence(List(Right(1), Left("error"), Right(3), Left("another error"))) shouldEqual Left("error")
  }

  "sequence(List())" should "return Right(List())" in {
    Either.sequence(List()) shouldEqual Right(List())
  }

}

