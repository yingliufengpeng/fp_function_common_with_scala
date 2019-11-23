package chapter02_Functional_datastruct


object Either_Data {

  def main(args: Array[String]): Unit = {

    sealed trait Either[+A, +B]
    object Either {
      def right[B](b: B) = Right(b)

      def left[A](a: A) = Left(a)

      def flatten[A, B](a: Either[A, Either[A, B]]): Either[A, B] = a match {
        case Right(p) => p
        case Left(x) => Left(identity(x))
      }

      def map[A, B, C](a: Either[A, B])(f: B => C): Either[A, C] = a match {
        case Right(x) => Right(f(x))
        case Left(y) => Left(identity(y))
      }

      def flatMap2[A, B, C](a: Either[A, B])(f: B => Either[A, C]): Either[A, C] = a match {
        case Right(x) => f(x)
        case Left(y) => Left(y)
      }

      def flatMap[A, B, C](a: Either[A, B])(f: B => Either[A, C]): Either[A, C] = flatten(map(a)(f))

      def map2[A, B, C, D](a: Either[A, B])(b: Either[A, C])(f: (B, C) => D): Either[A, D] = {
        (a, b) match {
          case (Right(x), Right(y)) => Right(f(x, y))
          case (Right(x), _) => Right(identity(x)).asInstanceOf[Either[A, Nothing]]
          case (Left(x), _) => Left(identity(x))
        }

      }

      def map3[A, B, C, D, E](a: Either[A, B])(b: Either[A, C])(c: Either[A, D])(f: (B, C, D) => C): Either[A, C] = {
        val m = map2(a)(b)((_, _))
        val n = map2(m)(c) { case ((x, y), z) => f(x, y, z) }
        n
      }


      def leftMap[A, B, C](a: Either[A, B])(f: A => C): Either[C, B] = a match {
        case Right(x) => Right(identity(x))
        case Left(y) => Left(f(y))
      }
    }

    object EitherImplicits {

      implicit class EitherOps[A](a: A) {
        def left = Left(a)

        def right = Right(a)
      }

      implicit class EitherMonoid[A, B](a: Either[A, B]) {

        def map[C](f: B => C): Either[A, C] = Either.map(a)(f)

        def leftMap[C](f: A => C): Either[C, B] = Either.leftMap(a)(f)

        def flatMap[C](f: B => Either[A, C]): Either[A, C] = Either.flatMap(a)(f)
      }

    }
    case class Left[+A, +B](a: A) extends Either[A, B]
    case class Right[+A, +B](b: B) extends Either[A, B]

    val r = Left(3)
    println(s"r is $r")

    import EitherImplicits._
    val r2 = 3.right
    println(s"r2 is $r2")

    val r3 = r2.map(_ + 2)
    println(s"r3 is $r3")

    val r4 = 4.left
    println(s"r4 is $r4")
    val r5 = r4.leftMap(_ + 1)
    println(s"r5 is $r5")

    val r6 = "56".right
    println(s"r7 is $r6")
    val r7 = r6.map(_.toInt + 33)
    println(s"r7 is $r7")

    def div(a: Int, b: Int): Either[Exception, Int] =
      try {
        (a / b).right
      }
      catch {
        case x: Exception => x.left
      }

    val r8 = div(1, 3)
    println(s"r8 is $r8")

    val r9 = div(1, 0)
    println(s"r9 is $r9")

    val r10 = r9.map(_ + 3)
    println(s"r10 is $r10")

    val r11 = r9.flatMap(_ => Right(44))
    println(s"r11 is $r11")

    val r12 = for {
      x <- r8
      y <- r9
    } yield x + y

    println(s"r12 is $r12")

  }
}
