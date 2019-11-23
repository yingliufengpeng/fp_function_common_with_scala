package chapter02_Functional_datastruct

object List_Data {


  def main(args: Array[String]): Unit = {

    sealed trait List[+A]
    object List {
      def empty[A]: List[A] = Nil

      def cons[A](a: A, tail: List[A]) = Cons(a, tail)

      def apply[A](as: A*): List[A] =
        if (as.isEmpty)
          empty
        else
          Cons(as.head, apply(as.tail: _*))

      def map[A, B](a: List[A])(f: A => B): List[B] = a match {
        case Nil => Nil
        case Cons(h, tail) => cons(f(h), map(tail)(f))
      }

      def map2[A, B, C](a: List[A])(b: List[B])(f: (A, B) => C): List[C] = map(zip(a)(b))(t => f(t._1, t._2))

      def map3[A, B, C, D](as: List[A])(bs: List[B])(cs: List[C])(f: (A, B, C) => D): List[D] =
        map2(map2(as)(bs)((_, _)))(cs) { case ((a, b), c) => f(a, b, c) }

      // list数据粘贴
      def :::[A](as: List[A], bs: List[A]): List[A] = (as, bs) match {
        case (Cons(head, tail), bs) => Cons(head, :::(tail, bs))
        case (Nil, bs) => bs
      }

      @scala.annotation.tailrec
      def drop[A](as: List[A], n: Int): List[A] = (as, n) match {
        case (Nil, _) => Nil
        case (Cons(_, tail), n) if n == 1 => tail
        case (Cons(_, tail), n) => drop(tail, n - 1)
      }

      def flatten[A](a: List[List[A]]): List[A] = a match {
        case Nil => Nil
        case Cons(heads, tail) => :::(heads, flatten(tail))
      }

      def zip[A, B](as: List[A])(bs: List[B]): List[(A, B)] = (as, bs) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(l, lt), Cons(r, rt)) => Cons((l, r), zip(lt)(rt))
      }

      def flatMap[A, B](a: List[A])(f: A => List[B]): List[B] = flatten(map(a)(f))

      def length[A](as: List[A]): Int = as match {
        case Nil => 0
        case Cons(_, tail) => 1 + length(tail)
      }

      @scala.annotation.tailrec
      def foldLeft[A, B](as: List[A])(zero: B)(f: (B, A) => B): B = as match {
        case Nil => zero
        case Cons(h, tail) => foldLeft(tail)(f(zero, h))(f)
      }

      def foldRight[A, B](as: List[A])(zero: B)(f: (A, B) => B): B = as match {
        case Nil => zero
        case Cons(h, tail) => f(h, foldRight(tail)(zero)(f))
      }

      // 通过foldleft来对foldright进行相应的实现
      def foldRight2[A, B](as: List[A])(zero: B)(f: (A, B) => B): B = foldLeft(as)(identity[B] _)((last, e) => {
        x: B => last(f(e, x))
      })(zero)


    }
    final case class Cons[+A](head: A, tail: List[A]) extends List[A]
    final case object Nil extends List[Nothing]

    object ImplicitList {

      implicit class List2Ops[A](v: A) {

        def empty: List[A] = Nil

      }

      implicit class List2Monad[A](v: List[A]) {

        def map[B](f: A => B): List[B] = List.map(v)(f)

        def map2[B, C](b: List[B])(f: (A, B) => C): List[C] = List.map2(v)(b)(f)

        def map3[B, C, D](b: List[B])(c: List[C])(f: (A, B, C) => D): List[D] = List.map3(v)(b)(c)(f)

        def flatMap[B](f: A => List[B]): List[B] = List.flatMap(v)(f)

        def length: Int = List.length(v)

        def drop(n: Int): List[A] = List.drop(v, n)

        def foldLeft[B](zero: B)(f: (B, A) => B): B = List.foldLeft(v)(zero)(f)

        def foldRight[B](zero: B)(f: (A, B) => B): B = List.foldRight(v)(zero)(f)

        def foldRight2[B](zero: B)(f: (A, B) => B): B = List.foldRight2(v)(zero)(f)
      }


    }

    val r = List.empty[Int]
    val r2 = List.cons(3, r)
    println(s"r is $r2")

    val r3 = List.cons(4, r2)
    println(s"r3 is $r3")

    val r4 = List.map(r3)(_ + 2)
    println(s"r4 is $r4")

    val r5 = List(1, 2, 3, 4, 5, 6)
    println(s"r5 is $r5")

    val r6 = List.flatMap(r5)(e => Cons(e, Cons(1, Nil)))
    println(s"r6 is $r6")

    val r7 = List.zip(r5)(r5)
    println(s"r7 is $r7")

    val r8 = List.map2(r5)(r5)(_ + _)
    println(s"r8 is $r8")

    import ImplicitList._

    val r9 = r8.map(_ + 1)
    println(s"r9 is $r9")

    val r10 = r9.map3(r9)(r9)(_ + _ + _)
    println(s"r10 is $r10")

    val r11 = r6.map(_ + 1)
    println(s"r11 is $r11")

    val r12 = List(1, 2, 3)
    val r13 = List(5, 6, 7)
    //    val r14 = List.:::(r12, r13)
    //    println(s"r14 is $r14")

    val r15 = r12.length
    println(s"r15 is $r15")

    val r16 = r9.drop(2)
    println(s"r16 is $r16")

    val r17 = r12.foldLeft(0)(_ + _)
    println(s"r17 is $r17")

    val r18 = r12.foldRight(0)(_ + _)
    println(s"r18 is $r18")

    val r19 = r12.foldRight(List.empty[Int])((h, tail) => Cons(h, tail))
    println(s"r19 is $r19")

    val r20 = r12.foldRight2(List.empty[Int])((h, tail) => Cons(h, tail))
    println(s"r19 is $r20")

  }
}
