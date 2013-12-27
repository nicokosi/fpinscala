package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We will later define other functions in terms of `nextInt`.
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) & // `&` is bitwise AND
                  ((1L << 48) - 1) // `<<` is left binary shift
      ((seed2 >>> 16).asInstanceOf[Int], // `>>>` is right binary shift with zero fill
       simple(seed2))
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

  import scala.math.abs  
  def positiveInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (Int.MinValue, next) => positiveInt(next)
    case (i, next) => (abs(i), next)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, next) = positiveInt(rng)
    val d = i / (Int.MaxValue.toDouble + 1)
    (d, next)
  }    

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(counter: Int, l: List[Int], rng: RNG) : (List[Int], RNG) = {
      if (counter == 0) {
        (l, rng)
      } else {
        val (i, rng2) = rng.nextInt
        go(counter - 1, i :: l, rng2)
      }      
    }
    go(count, List(), rng)
  }

  def positiveMax(n: Int): Rand[Int] = sys.error("todo")

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = sys.error("todo")

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
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
  def simulateMachine(inputs: List[Input]): State[Machine, Int] = sys.error("todo")
}

object StateMain {

  import fpinscala.state.RNG._
  def main(args: Array[String]): Unit = {
    // ex1
    val gen = simple(42)
    println("positive int: " + positiveInt(gen))
    println
    // ex2
    println("double [0..1[: " + double(gen))
    println
    // ex3
    println("intDouble: " + intDouble(gen))
    println("doubleInt: " + doubleInt(gen))
    println("double3: " + double3(gen))
    println
    // ex4
    println("10 random ints: " + ints(10)(gen))
    println
    // ex5
    def doubleViaMap =
      map(positiveInt)(i => i / (Int.MaxValue.toDouble + 1))
    println("double [0..1[ via map: " + doubleViaMap(gen))
    println
    // ex6
    def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
      map2(ra, rb)((_, _))
    val randIntDouble: Rand[(Int, Double)] =
      both(int, double)
    println("randIntDouble: " + randIntDouble(gen))
    println
    // ex8
    def positiveLessThan(n: Int): Rand[Int] =
      flatMap(positiveInt) { i =>
        val mod = i % n
        if (i + (n-1) - mod > 0) unit(mod)
        else positiveLessThan(n)
      }
    println("positiveLessThan 42: " + positiveLessThan(42)(gen))
    // ex 9
    def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s) { i => unit(f(i)) }
    def doubleViaMapViaFlatMap =
      mapViaFlatMap(positiveInt)(i => i / (Int.MaxValue.toDouble + 1))
    println("doubleViaMapViaFlatMap : " + doubleViaMapViaFlatMap(gen))
  }

}