package chapter03_Strictness_and_Laziness

object Stream_Data {


  def main(args: Array[String]): Unit = {

    sealed trait Stream[+A]

    object Stream {

      def empty[A]: Stream[A] = Empty

      // 这块的语法看起来有意思了!!!
      def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
        // 如果是使用lazy的话,那么head,tail中的数据还是会继续计算的,所以我们
        // 需要设定成为lazy
        lazy val h = head
        lazy val t = tail
        Cons(() => h, () => t)
      }

      def apply[A](as: A*): Stream[A] =
        if (as.isEmpty)
          Empty
        else
          cons(as.head, apply(as.tail: _*))

      def toList[A](as: Stream[A]): List[A] = as match {
        case Empty => Nil
        case Cons(h, tail) => h() :: toList(tail())
      }

      def foldRight[A, B](as: Stream[A])(zero: B)(f: (=> B, A) => B): B = as match {
        case Empty => zero
        case Cons(h, tail) => f(foldRight(tail())(zero)(f),h())
      }

      // 给定一个初始的状态还有一个方法,然后使用下一个状态做下一个值的处理的逻辑
      // 但是 S => (A, S) 会永远不停止,所以我们使用Option来做介入
      def unfold[A, S](zero: S)(f: S => Option[(A, S)]): Stream[A] = f(zero) match {
        case None => Empty
        case Some((a, s1)) => cons(a, unfold(s1)(f))
      }

      def take[A](as: Stream[A])(n: Int): Stream[A] = unfold((as, n)){
        case (Empty, _) => None
        case (_, 0) => None
        case (Cons(h, tai), k) => Some((h(),(tai(), k - 1)))
      }

    }

    object StreamImplicit {



      implicit class Stram2Monoid[A](as: Stream[A]) {

        def toList: List[A] = Stream.toList(as)

        def foldRight[B](zero: B)(f: (=> B, A) => B): B = Stream.foldRight(as)(zero)(f)

        def take(n: Int): Stream[A] = Stream.take(as)(n)

      }
    }

    case object Empty extends Stream[Nothing]

    case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A] {
      override def toString: String = "head tail"
    }

    val r = Stream(3, 4, 5)
    println(s"r is $r")

    val r2 = Stream.toList(r)
    println(s"r2 is $r2")

    import StreamImplicit._
    val r3 = r.toList
    println(s"r3 is $r3")

    val r4 = r.foldRight(0)(_ + _)
    println(s"r4 is $r4")

    println("*" * 50)
    val r5 = r.foldRight(false)((acc, e) => { println(s"e is $e"); e == 4 || acc})
    println(s"r5 is $r5")

//    val r6 = Stream((1 to Int.MaxValue).toList:_*)
//    println(s"r6 is $r6")

//    lazy val ones: Stream[Int] = Stream.cons(1, ones)
//    println(s"ones is $ones")
//    ones.toList

    val r6 = Stream.unfold(1)(s => Some(1, s))
    println(s"r6 is $r6")

    val r7 = r3.take(-1)
    println(s"r7 is $r7")

  }
}
