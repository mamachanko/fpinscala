import org.scalatest.{FunSpec, Matchers}

class RNGSpec extends FunSpec with Matchers {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  object AlwaysZero extends SimpleRNG(123) {
    override def nextInt: (Int, RNG) = (0, AlwaysZero)
  }

  object RNG {
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (n, nextRng) = rng.nextInt
      (Math.abs(n), nextRng)
    }

    def double(rng: RNG): (Double, RNG) = {
      val (i, r) = nonNegativeInt(rng)
      (i / (Int.MaxValue.toDouble + 1), r)
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, nextRng) = rng.nextInt
      val (d, _) = double(rng)
      ((i, d), nextRng)
    }
  }

  describe("RNG") {
    describe("given a seed") {
      it("generates an integer consistently") {
        val (n, _) = SimpleRNG(42).nextInt
        n shouldBe an[Integer]
        n shouldEqual SimpleRNG(42).nextInt._1
      }
    }

    describe("nonNegativeInt") {

      describe("given an RNG that produces a negative number") {
        // produces -1281479697
        val (_, rng) = SimpleRNG(42).nextInt

        it("returns a non-negative in between 0 and the Int.maxValue") {
          val (n, _) = RNG.nonNegativeInt(rng)
          n shouldEqual 1281479697
        }

        it("returns another RNG") {
          val (_, nextRng) = RNG.nonNegativeInt(rng)
          nextRng.nextInt._1 should not equal rng.nextInt._1
        }
      }

      describe("given an RNG that produces 0") {

        it("returns 0") {
          val (n, _) = RNG.nonNegativeInt(AlwaysZero)
          n shouldEqual 0
        }
      }
    }

    describe("double") {
      describe("given some RNG") {
        val rng = SimpleRNG(42)
        it("returns a double between 0 and 1") {
          val (n, _) = RNG.double(rng)
          n shouldBe >=(0.0)
          n shouldBe <(1.0)
        }
      }
    }

    describe("intDouble") {
      describe("given some RNG") {
        val rng = SimpleRNG(42)

        it("returns an int-double tuple") {
          val ((i, d), _) = RNG.intDouble(rng)
          println(rng.nextInt)
          i shouldEqual 16159453
          d shouldEqual 0.007524831686168909
        }

        it("returns a new RNG") {
          val (_, nextRNG) = RNG.intDouble(rng)
          nextRNG shouldNot equal(rng)
        }
      }
    }
  }

}