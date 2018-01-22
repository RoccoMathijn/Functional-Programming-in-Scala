object CH6 {
   type Rand[+A] = RNG => (A, RNG)
//  type Rand[A] = State[RNG, A]

  trait RNG {
    def nextInt: (Int, RNG)

    val int: Rand[Int] = _.nextInt
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  object RNG {
    def unit[A] (a: A): Rand[A] = rng => (a, rng)

    def map[A, B](s: Rand[A]) (f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }
  }

  // Exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (randomInt, newRNG): (Int, RNG) = rng.nextInt
    val nonNegative = Math.abs(randomInt)

    if (nonNegative == Int.MinValue) {
      nonNegativeInt(newRNG)
    } else {
      (nonNegative, newRNG)
    }
  }

  // Exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (randomInt, newRNG): (Int, RNG) = nonNegativeInt(rng)

    if (randomInt == Int.MaxValue) {
      double(newRNG)
    } else {
      (randomInt.toDouble / Int.MaxValue, newRNG)
    }
  }

  // Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (randomInt, newRNG): (Int, RNG) = nonNegativeInt(rng)
    val (randomDouble, newRNG2): (Double, RNG)  = double(newRNG)

    ((randomInt, randomDouble), newRNG2)
  }

  def doubleInt(rng: RNG): ((Double, Int),  RNG) = {
    val (randomDouble, newRNG): (Double, RNG)  = double(rng)
    val (randomInt, newRNG2): (Int, RNG) = nonNegativeInt(newRNG)

    ((randomDouble, randomInt), newRNG2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (randomDouble1, newRNG): (Double, RNG) = double(rng)
    val (randomDouble2, newRNG2): (Double, RNG)  = double(newRNG)
    val (randomDouble3, newRNG3): (Double, RNG)  = double(newRNG2)

    ((randomDouble1, randomDouble2, randomDouble3), newRNG3)
  }

  // Exercise 6.4
  def ints(count: Int) (rng: RNG): (List[Int], RNG) = {
    def loop(count: Int, acc: (List[Int], RNG)): (List[Int], RNG) = {
      if (count == 0) acc
      else {
        val (randomInt: Int, newRNG: RNG) = nonNegativeInt(acc._2)
        loop(count - 1, (randomInt :: acc._1, newRNG))
      }
    }

    loop(count, (List.empty, rng))
  }

  // Exercise 6.5
  import RNG._
  def double2(rng: RNG): Rand[Double] =
    map(nonNegativeInt) ((x: Int) => x.toDouble)

  // Exercise 6.6
  def map2[A,B,C] (ra: Rand[A], rb: Rand[B]) (f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }
  }

  // Exercise 6.7
  def sequence[A] (fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      fs.foldRight((List.empty[A], rng))((ra, acc) => {
        val (a: A, rng2: RNG) = ra(acc._2)
        (a :: acc._1, rng2)
      })
    }
  }

  // Exercise 6.8
  def flatMap[A, B](f: Rand[A]) (g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
     flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    })
  }

  // Exercise 6.9
  def mapInTermsOfFlatmap[A, B] (s: Rand[A]) (f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  def map2InTermsOfFlatmap[A,B,C] (ra: Rand[A], rb: Rand[B]) (f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a,b))))
  }

  // Exercise 6.10

  import State._

  case class State[S, +A] (run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))

    def flatMap[B] (f: A => State[S, B]): State[S, B] =
      State(s => {
        val (a: A, s2: S) = run(s)
        f(a).run(s2)
      })
  }

  object State {
    def unit[S, A](a: A): State[S, A] =
      State(s => (a, s))

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
      fs.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    } yield ()

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  }

  // Exercise 6.11
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  object Machine {

    def run: Input => Machine => Machine = (input: Input) => (s: Machine) => {
      (input, s) match {
        case(_, Machine(_, 0, _)) => s
        case (Coin, Machine(false, _, _)) => s
        case (Turn, Machine(true, _, _)) => s
        case (Coin, Machine(true, candies, coins)) => Machine(locked = false, candies, coins + 1)
        case (Turn, Machine(false, candies, coins)) => Machine(locked = true, candies - 1, coins)
      }
    }

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
      _ <- State.sequence(inputs.map(modify[Machine] _ compose run))
      s <- get
    } yield (s.coins, s.candies)

  }
}
