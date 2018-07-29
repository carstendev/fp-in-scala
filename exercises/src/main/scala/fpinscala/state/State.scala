package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n ,nextRng) = rng.nextInt
    (if (n < 0) -(n + 1) else n, nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n ,nextRng) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), nextRng)
  }

  def doubleViaMap(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))(rng)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (intR, rng1) = nonNegativeInt(rng)
    val (doubleR, rng2) = double(rng1)
    ((intR, doubleR), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (doubleR, rng1) = double(rng)
    val (intR, rng2) = nonNegativeInt(rng1)
    ((doubleR, intR), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (doubleR, rng1) = double(rng)
    val (doubleR2, rng2) = double(rng1)
    val (doubleR3, rng3) = double(rng2)
    ((doubleR, doubleR2, doubleR3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    Seq.fill(count)(Nil).foldLeft((List.empty[Int], rng)){ (a, _) =>
      val (i, r) = a._2.nextInt
      (a._1 :+ i, r)
    }
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val(i, r1) = ra(rng)
      val(i2, r2) = rb(r1)
      (f(i,i2), r2)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
      fs.foldRight(unit(List[A]())){ (f, acc) =>
        map2(f, acc)(_ :: _)
    }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
