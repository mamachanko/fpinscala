import org.scalatest.{FunSpec, Matchers}

class StreamSpec extends FunSpec with Matchers {

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

    def take(n: Int): Stream[A] = Stream.unfold((this, n)) {
      case (Cons(h, t), i) if i > 1 => Some((h(), (t(), i - 1)))
      case (Cons(h, _), i) if i == 1 => Some((h(), (Stream.empty, 0)))
      case _ => None
    }

    def takeWhile(p: A => Boolean): Stream[A] = Stream.unfold(this) {
      case Empty => None
      case Cons(h, t) => if (p(h())) Some((h(), t())) else None
    }

    //      foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else Empty)

    def drop(n: Int): Stream[A] = this match {
      case Cons(h, t) if n == 1 => t()
      case Cons(h, t) if n > 1 => t().drop(n - 1)
      case _ => Stream.empty
    }

    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

    def map[B](f: A => B): Stream[B] = Stream.unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }

    //      foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((a, b) => f(a).append(b))

    def filter(f: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, b) => if (f(a)) Stream.cons(a, b) else b)

    def append[B >: A](as: => Stream[B]): Stream[B] = foldRight(as)((a, b) => Stream.cons(a, b))

    def zipWith[B, AA >: A](that: Stream[AA])(f: (A, AA) => B): Stream[B] = Stream.unfold((this, that)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

    def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, that)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case _ => None
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = {
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    }

    def ones: Stream[Int] = constant(1)

    def constant[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

    def from(n: Int): Stream[Int] = unfold(n)(i => Some((i, i + 1)))

    def fibs(): Stream[Int] = unfold((0, 1))(i => Some((i._1, (i._2, i._1 + i._2))))

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case None => empty
      case Some((nextValue, nextState)) => cons(nextValue, unfold(nextState)(f))
    }
  }

  describe("Stream") {

    describe("given an empty Stream") {
      val stream = Stream.empty

      describe("when turning it into a List") {
        it("returns an empty List") {
          stream.toList shouldEqual List()
        }
      }

      describe("when taking the first n elements") {
        it("returns an empty Stream") {
          stream.take(2) shouldEqual Stream.empty
        }
      }

      describe("when dropping the first n elements") {
        it("returns an empty Stream") {
          stream.drop(2) shouldEqual Stream.empty
        }
      }

      describe("when taking the first n elements matching a predicate") {
        it("returns an empty Stream") {
          Stream.empty[Int].takeWhile(_ % 2 != 0) shouldEqual Stream.empty
        }
      }
      describe("when checking a predicate for all elements") {
        it("returns true") {
          Stream.empty[Int].forAll(_ > 0) shouldEqual true
        }
      }

      describe("when mapping a function") {
        it("returns an empty stream") {
          Stream.empty[Int].map(_ * 2) shouldEqual Stream.empty
        }
      }

      describe("when filtering") {
        it("returns an empty stream") {
          Stream.empty[Int].filter(_ % 2 == 0) shouldEqual Stream.empty
        }
      }

      describe("when appending") {
        it("returns stream with the other elements") {
          Stream.empty[Int].append(Stream(6, 5, 4)).toList shouldEqual Stream(6, 5, 4).toList
        }
      }

      describe("when flatMap'ing a function") {
        it("returns an empty stream") {
          Stream.empty[Int].flatMap(i => Stream(i, i)) shouldEqual Stream.empty
        }
      }

      describe("when zipping with another Stream") {
        it("returns an empty Stream") {
          Stream.empty[Int].zipWith(Stream(1, 2, 3))(_ + _) shouldEqual Stream.empty
          Stream(1, 2, 3).zipWith(Stream.empty[Int])(_ + _) shouldEqual Stream.empty
        }
      }

    }

    describe("given a non-empty Stream") {
      val stream = Stream(1, 2, 3)

      describe("when turning it into a List") {
        it("returns a List") {
          stream.toList shouldEqual List(1, 2, 3)
        }
      }

      describe("when taking the first n elements") {
        it("returns a Stream containing the first n elements") {
          stream.take(2).toList shouldEqual Stream(1, 2).toList
        }
      }

      describe("when dropping the first n elements") {
        it("returns a Stream without the first n elements") {
          stream.drop(2).toList shouldEqual Stream(3).toList
        }
      }

      describe("when taking the first n elements matching a predicate") {
        it("returns a Stream consisting of the first n elements matching a predicate") {
          stream.takeWhile(_ % 2 != 0).toList shouldEqual Stream(1).toList
        }
      }

      describe("when checking a predicate for all elements") {
        describe("when all elements satisfy the predicate") {
          it("returns true") {
            stream.forAll(_ > 0) shouldEqual true
          }
        }
        describe("when one element does not satisfy the predicate") {
          it("returns false") {
            stream.forAll(_ < 2) shouldEqual false
          }
        }
      }

      describe("when mapping a function") {
        it("returns a stream with the function applied to every element") {
          stream.map(_ * 2).toList shouldEqual List(2, 4, 6)
        }
      }

      describe("when filtering") {
        it("returns a stream excluding elements") {
          stream.filter(_ % 2 == 0).toList shouldEqual List(2)
        }
      }

      describe("when appending") {
        it("returns a stream composed of the two streams") {
          stream.append(Stream(6, 5, 4)).toList shouldEqual List(1, 2, 3, 6, 5, 4)
        }
      }

      describe("when flatMap'ing a function") {
        it("returns a stream containing all the results") {
          stream.flatMap(i => Stream(i, i)).toList shouldEqual List(1, 1, 2, 2, 3, 3)
        }
      }

      describe("when zipping with another Stream") {
        it("returns a zip'd Stream") {
          Stream(1, 2, 3).zipWith(Stream("1", "2", "3"))(_ -> _).toList shouldEqual Stream((1, "1"), (2, "2"), (3, "3")).toList
        }

        it("returns a zip'd Stream no longer than the shorter Stream") {
          Stream(1, 2, 3, 4).zipWith(Stream("1", "2", "3"))(_ -> _).toList shouldEqual Stream((1, "1"), (2, "2"), (3, "3")).toList
          Stream(1, 2, 3).zipWith(Stream("1", "2", "3", "4"))(_ -> _).toList shouldEqual Stream((1, "1"), (2, "2"), (3, "3")).toList
        }
      }

      describe("when zipAll with another Stream") {
        it("returns a zip'd Stream") {
          Stream(1, 2, 3).zipAll(Stream("1", "2", "3")).toList shouldEqual Stream((Some(1), Some("1")), (Some(2), Some("2")), (Some(3), Some("3"))).toList
        }

        it("returns a zip'd Stream until the longer Stream is exhausted") {
          Stream(1, 2, 3, 4).zipAll(Stream("1", "2", "3")).toList shouldEqual Stream((Some(1), Some("1")), (Some(2), Some("2")), (Some(3), Some("3")), (Some(4), None)).toList
          Stream(1, 2, 3).zipAll(Stream("1", "2", "3", "4")).toList shouldEqual Stream((Some(1), Some("1")), (Some(2), Some("2")), (Some(3), Some("3")), (None, Some("4"))).toList
        }
      }
    }

    describe("ones") {
      it("returns ones") {
        Stream.ones.take(5).toList shouldEqual Seq.fill(5)(1).toList
      }
    }

    describe("constant") {
      it("returns constants") {
        Stream.constant(2).take(5).toList shouldEqual Seq.fill(5)(2).toList
      }
    }

    describe("from") {
      it("returns integers starting from") {
        Stream.from(12).take(5).toList shouldEqual List(12, 13, 14, 15, 16)
      }
    }

    describe("fibs") {
      it("returns Fibonacci numbers") {
        Stream.fibs().take(7).toList shouldEqual List(0, 1, 1, 2, 3, 5, 8)
      }
    }

    describe("unfold") {
      describe("given an initial state a forever producing function") {
        it("produces") {
          Stream.unfold(1)(i => Some((Math pow(2, i), i + 1))).take(6).toList shouldEqual List(2, 4, 8, 16, 32, 64)
        }
      }

      describe("given an initial state a terminating producing function") {
        it("produces") {
          Stream.unfold(1)(i => if (i > 3) None else Some(Math pow(2, i), i + 1)).take(120).toList shouldEqual List(2, 4, 8)
        }
      }
    }

  }
}
