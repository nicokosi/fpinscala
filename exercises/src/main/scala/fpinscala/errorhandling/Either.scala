package fpinscala.errorhandling

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
  case Left(e) => Left(e)
  case Right(a) => Right(f(a))
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
  case Left(e) => Left(e)
  case Right(a) => f(a)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = sys.error("todo")

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = sys.error("todo")
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!") 
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Double, y: Double): Either[Exception, Double] = 
    try {
      Right(x / y)
    } catch {
      case e: Exception => Left(e)
    }

}

object EitherMain {

  import fpinscala.errorhandling.Either._
  def main(args: Array[String]): Unit = {
    println("map(82/2 + 1): " + (safeDiv(82, 2) map (_ + 1)))
    println("map(42/0 + 1): " + (safeDiv(42, 0) map (_ + 1)))
    println()

    def only42(d:Double): Either[String, Double] = 
      if (d == 42) Right(42) else Left("Not 42")
    println("flatMap: " + (safeDiv(84, 2) flatMap only42))
    println("flatMap: " + (safeDiv(2, 1) flatMap only42))
    println()
  }

}