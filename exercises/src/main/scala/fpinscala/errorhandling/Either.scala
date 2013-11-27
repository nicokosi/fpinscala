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

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
  case Left(_) => b
  case Right(_) => this
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
  this flatMap (x => (b flatMap (y => Right(f(x, y)))))

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
    val meanOK = mean(Array(82, 0))
    val noMean = mean(Array[Double]())

    println("map mean(82,0) + 1: " + (meanOK map (_ + 1)))
    println("map mean(emptyList) + 1: " + (noMean map (_ + 1)))
    println()

    def only42(d:Double): Either[String, Double] = 
      if (d == 42) Right(42) else Left("Not 42")
    println("flatMap mean(82,0) only42: " + (meanOK flatMap only42))
    println("flatMap mean(emptyList) only42: " + (noMean flatMap only42))
    println()

    println("orElse mean(emptyList) or 42: " + (noMean orElse Right(42)))
    println("orElse mean(emptyList) or custom error: " + (noMean orElse Left("Custom error")))
    println()

    val divisionOK = safeDiv(4, 2)
    val divisionKO = safeDiv(4, 0)
    def add(a: Double, b: Int) : Double = a + b
    println("safe div: 4/2 + 1: " + (divisionOK.map2(Right(1))(add)))
    println("safe div 4/2 + missing: " + (divisionOK.map2(Left("missing add operand"))(add)))
    println("safe div 4/0 + 1: " + (divisionKO.map2(Right(1))(add)))
    println()
  }

}