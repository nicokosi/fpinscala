package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We will later define other functions in terms of `nextInt`.
}

object RNG {
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

  def positiveInt(rng: RNG): (Int, RNG) = {
    // exercise 1:
    val (num, gen) = rng.nextInt
    if (num == Integer.MAX_VALUE) 
      positiveInt(gen)
    else 
      (num.abs, gen)
  }

  def double(rng: RNG): (Double, RNG) = {    
    /*
    // exercise 2 (raw implementation):
    val (n, rng2) = positiveInt(rng)
    val d = n.toDouble / Integer.MAX_VALUE
    (d, rng2)
    */

    // exercise 5 (more elegant implementation using 'map' combinator):
    map(positiveInt)(n => n.toDouble / Integer.MAX_VALUE)(rng)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    /*
    // exercise 3 (raw implementation):
    val (i, rng2) = positiveInt(rng)
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
    */

    // exercise 6 (more elegant implementation using 'map2' combinator):
    both(positiveInt, double)(rng)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    /*
    // exercise 3 (raw implementation):
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
    */

    // exercise 6 (more elegant implementation using 'map2' combinator):
    both(double, positiveInt)(rng)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))


  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    // exercise 3
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = { // exercise 4
    @annotation.tailrec
    def addInt(rng: RNG, count: Int, list: List[Int]): (List[Int], RNG) = {
      if (count == 0) (list, rng)
      else {
        val (i, rng2) = positiveInt(rng)
        addInt(rng2, count - 1, i :: list)
      }
    }
    addInt(rng, count, List())
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    // exercise 6
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a,b), rng3)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}