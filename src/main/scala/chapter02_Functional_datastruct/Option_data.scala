package chapter02_Functional_datastruct

object Option_data {

  def main(args: Array[String]): Unit = {

    sealed trait Option[+A]
    object Option {
      import OptionImplicit._

      def apply[T](t: T): Option[T] = Some(t)

      def empty[T]: None.type = None

      def product[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
        (a, b) match {
          case (Some(x), Some(y)) => Some(x, y)
          case _ => None
        }

      def map[A, B](a: Option[A])(f: A => B): Option[B] = a match {
        case Some(v) => Option(f(v))
        case None => None
      }

      def map2[A, B, C](a: Option[A])(b: Option[B])(f: (A, B) => C): Option[C] = {
        map(product(a, b)) { case (x, y) => f(x, y)}}

      def map3[A, B, C, D](a: Option[A])(b: Option[B])(c: Option[C])(f: (A, B, C) => D): Option[D] = {
//        val m = map2(a)(b)((_, _))
//        map2(m)(c)((x, y) => f(x._1, x._2, y))
        map2(map2(a)(b)((_, _)))(c){case ((x, y), z) => f(x, y, z)}
      }

      def map4[A, B, C, D, E](a: Option[A])(b: Option[B])(c: Option[C])(d: Option[D])(f: (A, B, C, D) => E): Option[E] =  for {
        x <- a
        y <- b
        z <- c
        w <- d
      } yield f(x, y, z, w)

      def flatten[A](t: Option[Option[A]]): Option[A] = t match {case Some(t) => t}

      def flatMap[A, B](a: Option[A])(f: A => Option[B]): Option[B] = flatten(map(a)(f))

      def filter[A](a: Option[A])(f: A => Boolean): Option[A] = a match {
        case Some(x) if f(x) => Some(x)
        case _ => None
      }
    }
    object OptionImplicit {

      implicit class value2Opton[T](t: T) {
        def some = Option(t)

        def empty[A]: None.type = None

      }

      implicit class Option2Map[A](t: Option[A]) {
        def map[B](f: A => B): Option[B] = Option.map(t)(f)
        def flatMap[B](f: A => Option[B]): Option[B] = Option.flatMap(t)(f)
        def filter(f: A => Boolean): Option[A] = Option.filter(t)(f)
      }

    }
    case object None extends Option[Nothing]
    case class Some[+T](value: T) extends Option[T]

    val m = Option(44)
    val n = Option.empty[Int]

    println(s"m is $m, n is $n")

    import OptionImplicit._
    val r = 1.some
    val r2 = 2.empty
    println(s"r is $r, r2 is $r2")

    val r3 = Option.map(r)(_ + 1)
    println(s"r3 is $r3")

    val r4 = 44.some
    println(s"r4 is $r4")

    val r5 = 33.some
    println(s"r5 is $r5")

    val r6 = Option.map2(r4)(r5)(_ + _)
    println(s"r6 is $r6")

    val r7 = Option.map3(r4)(r5)(r6)(_ + _ + _)
    println(s"r7 is $r7")

    val r8 = r7.flatMap(e => Option(e))
    println(s"r8 is $r8")

    val r9 = for {
      x <- r7
      y <- r8
    } yield x + y
    println(s"r9 is $r9")

    val r10 = r9.filter(_ % 2 == 0)
    println(s"r10 is $r10")

     val r11 = Option.map4(r7)(r8)(r9)(r10)(_ + _ + _ + _)
    println(s"r11 is $r11")


  }

}
