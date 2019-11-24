package chapter07_Monad

import scala.language.higherKinds

//import scala.language.postfixOps
object Monad {

  def main(args: Array[String]): Unit = {
    // F[A]  * -> * HKT   * -> * -> *
    trait Functor[F[_]] {
      // 两个具体的小型范畴的映射
      def map[A, B](fa: F[A])(f: A => B): F[B]

      def lift[A, B](f: A => B): F[A] => F[B] = fa => map(fa)(f)
    }

    case class NonEmptyList[A](head: A, tail: Option[NonEmptyList[A]]) {
      def toList: List[A] = head :: (tail match {
        case Some(p) => p.toList
        case None => Nil
      })

//      def map[B](f: A => B): NonEmptyList[B] = this match {
//        case NonEmptyList(h, Some(tail)) => NonEmptyList(f(h), Some(tail.map(f)))
//        case NonEmptyList(h, None) => NonEmptyList(f(h), None)
//      }

    }

    object NonEmptyList {

      def ::[A](left: NonEmptyList[A], right: NonEmptyList[A]): NonEmptyList[A] = left match {
        case NonEmptyList(h, None) => NonEmptyList(h, Some(right))
        case NonEmptyList(h, Some(tail)) => NonEmptyList(h, Some(::(tail, right)))
      }

      def flatten[A](fa: NonEmptyList[NonEmptyList[A]]): NonEmptyList[A] = fa match {
        case NonEmptyList(p, None) => p
        case NonEmptyList(head_list, Some(tail)) => ::(head_list, flatten(tail))
      }

      def ap[A, B](fab: NonEmptyList[A => B])(fa: NonEmptyList[A]): NonEmptyList[B] = (fab, fa) match {
        case (NonEmptyList(f, _), NonEmptyList(h, Some(p))) => NonEmptyList(f(h), Some(ap(fab)(p)))
        case (NonEmptyList(f, _), NonEmptyList(h, None)) => NonEmptyList(f(h), None)
      }

      def map[A, B](fa: NonEmptyList[A])(f: A => B): NonEmptyList[B] = fa match {
        case NonEmptyList(h, Some(tail)) => NonEmptyList(f(h), Some(map(tail)(f)))
        case NonEmptyList(h, None) => NonEmptyList(f(h), None)
      }
    }

    object Functor {

      def apply[F[_] : Functor]: Functor[F] = implicitly[Functor[F]]

      trait implicits {

        implicit val nonEmptyListFunctor: Functor[NonEmptyList] = new Functor[NonEmptyList] {
          override def map[A, B](fa: NonEmptyList[A])(f: A => B): NonEmptyList[B] = fa match {
            case NonEmptyList(head, None) => NonEmptyList(f(head), None)
            case NonEmptyList(head, Some(tail)) => NonEmptyList(f(head), Some(map(tail)(f)))
          }
        }

      }

      object implicits extends implicits

    }

    trait Applicative[F[_]] extends Functor[F] {

      def pure[A](a: A): F[A]

      override def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)

      def map2[A, B, C](fa: F[A])(fb: F[B])(f: (A, B) => C): F[C] = ap(map(fa)(f.curried(_)))(fb)

      def map3[A, B, C, D](fa: F[A])(fb: F[B])(fc: F[C])(f: (A, B, C) => D): F[D] = ap(map2(fa)(fb)(f.curried(_)(_)))(fc)

      def ap[A, B](fab: F[A => B])(fa: F[A]): F[B]
    }

    object Applicative {
      def apply[F[_]](implicit applicative: Applicative[F]): Applicative[F] = applicative

      trait implicits {
        implicit val nonEmptyListAppliciate: Applicative[NonEmptyList] = new Applicative[NonEmptyList] {
          override def pure[A](a: A): NonEmptyList[A] = NonEmptyList(a, None)

          override def ap[A, B](fab: NonEmptyList[A => B])(fa: NonEmptyList[A]): NonEmptyList[B] = NonEmptyList.ap(fab)(fa)

          override def map[A, B](fa: NonEmptyList[A])(f: A => B): NonEmptyList[B] = NonEmptyList.map(fa)(f)
        }
      }

      object implicits extends implicits
    }

    trait Monad[F[_]] extends Applicative[F] {
//      override def pure[A](a: A): F[A] = ???

      def flatten[A](fa: F[F[A]]): F[A]

      def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = flatten(map(fa)(f))

//      def liftF[A, B](f: A => B): A => F[B] = a => map(pure(a))(f(_))
      def liftF[A, B](f: A => B): A => F[B] = a => pure(f(a))

//      override def ap[A, B](fab: F[A => B])(fa: F[A]): F[B] = flatMap(fa)(a => {
//        map(fab)(f => f(a))
//      })

//      override def ap[A, B](fab: F[A => B])(fa: F[A]): F[B] = flatten(map(fab)(f => map(fa)(e => f(e))))


//      override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(liftF(f))
    }

    object Monad {

      def apply[F[_]](implicit monad: Monad[F]): Monad[F] = monad

      trait implicits {

        implicit val nonEmptyList2Monad: Monad[NonEmptyList] = new Monad[NonEmptyList] {
          override def flatten[A](fa: NonEmptyList[NonEmptyList[A]]): NonEmptyList[A] = NonEmptyList.flatten(fa)

          override def ap[A, B](fab: NonEmptyList[A => B])(fa: NonEmptyList[A]): NonEmptyList[B] = NonEmptyList.ap(fab)(fa)

          override def pure[A](a: A): NonEmptyList[A] = NonEmptyList(a, None)
        }

      }

      object implicits extends implicits {

        implicit class xxx[F[_]: Monad, A](x: F[A])(implicit monad: Monad[F]) {
//          val monad = implicitly[Monad[F]]
          def map[B](f: A => B): F[B] = monad.map(x)(f)
          def flatMap[B](f: A => F[B]): F[B] = monad.flatMap(x)(f)
        }
      }
    }

    import Functor.implicits._
    val r = NonEmptyList(1, Some(NonEmptyList(2, Some(NonEmptyList(3, None)))))
    val functor = Functor[NonEmptyList]
    val r2 = functor.map(r)(_ + 3)
    println(s"r is ${r.toList}")
    println(s"r2 is ${r2.toList}")

    val f1: Int => Int = a => a + 3

    val f2: NonEmptyList[Int] => NonEmptyList[Int] = functor.lift(f1)

    // 理论上两者之间是相等的关系型
    // 用把低纬度的映射提升到高纬度的映射之中
    val r33 = functor.map(r)(f1)
    val r44 = f2(r)

    println(s"r33 is ${r33.toList}")
    println(s"r44 is ${r44.toList}")
    println(s"r33 == r44 is ${r33 == r44}")

    import Applicative.implicits._

    val applicative = Applicative[NonEmptyList]

    val r7 = NonEmptyList.::(r, r)
    println(s"r7 is ${r7.toList}")

    val r8 = NonEmptyList("wang", Some(NonEmptyList("peng", Some(NonEmptyList("表单验证", Some(NonEmptyList("程序错误", None)))))))

    val r9 = applicative.map2(r8)(r8)(_ -> _)
    println(s"r9 is ${r9.toList}")

    import Monad.implicits._
    val monad = Monad[NonEmptyList]
    val r5 = monad.flatMap(r)(e => NonEmptyList(e, Some(NonEmptyList(e * 20, None))))
    val r6: List[Int] = r5.toList
    println(s"r6 is ${r6}")

    val r11 = for {
      a <- r
      b <- r
    } yield a + b

    println(s"r11 is ${r11.toList}")

  }

}
