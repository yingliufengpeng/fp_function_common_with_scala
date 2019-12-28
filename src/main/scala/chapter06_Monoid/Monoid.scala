package chapter06_Monoid
import scala.language.higherKinds

object Monoid {

  def main(args: Array[String]): Unit = {
    /**
     *  ** Monoid
     *  *** class 一种分类  许多的实例 object  对象之间的关系 + (map morphism)
     *  *** axioms 结合律 交换律 identity(同一律)
     */

    trait SemiGroup[A] {
      def op(x: A)(y: A): A   // 半群满足某种性质,是具有封闭的操作
    }

    trait Monoid[A] extends SemiGroup[A] {
      def identity: A         // 幺半群则是有一个单位元
      def zero: A = identity
    }

    trait Foldable[F[_]] {

      def foldLeft[A](as: F[A])(zero: A)(f: (A, A) => A): A

    }

    object MonoidInstances {

      def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid

      implicit val intAddtitionMonoid: Monoid[Int] = new Monoid[Int] {
        override def identity: Int = 0
        override def op(x: Int)(y: Int): Int = x + y
      }

//      implicit val intMutiplyMonoid: Monoid[Int] = new Monoid[Int] {
//        override def identity: Int = 1
//        override def op(x: Int)(y: Int): Int = x * y
//      }

      implicit val stringConcatMonoid: Monoid[String] = new Monoid[String] {
        override def identity: String = ""
        override def op(x: String)(y: String): String = x + y
      }

      implicit val ListFoldable: Foldable[List] = new Foldable[List] {
        override def foldLeft[A](as: List[A])(zero: A)(f: (A, A) => A): A = as.foldLeft(zero)(f)
      }

      implicit val OptionFoldable: Foldable[Option] = new Foldable[Option] {
        override def foldLeft[A](as: Option[A])(zero: A)(f: (A, A) => A): A = as.foldRight(zero)(f)
      }

      def sum[A](as: List[A])(implicit monoid: Monoid[A]): A = as.foldLeft(monoid.zero)(monoid.op(_)(_))

      def fold[F[_]: Foldable, A: Monoid](as: F[A]): A = {
        val foldable = implicitly[Foldable[F]]
        val monoid = implicitly[Monoid[A]]
        foldable.foldLeft(as)(monoid.zero)(monoid.op(_)(_))
      }
    }

    object MonoidLaw {
      // 结合律
      def associativeLaw[A](x: A, y: A, z: A)(implicit monoid: Monoid[A]): Boolean =
        monoid.op(monoid.op(x)(y))(z) == monoid.op(x)(monoid.op(y)(z))

      def zeroLaw[A](x: A)(implicit monoid: Monoid[A]): Boolean =
        monoid.op(x)(monoid.zero) == monoid.op(monoid.zero)(x)

      //

    }

    import MonoidInstances._
    val r = MonoidInstances.sum(List("44", "55", "66"))
    println(s"r is $r")

    val r2 = MonoidInstances.fold(List(1, 2, 3))
    println(s"r2 is $r2")

    val r3 = MonoidInstances.fold(List("3", "4", "5"))
    println(s"r3 is $r3")

    val r4 = MonoidLaw.associativeLaw(3, 4, 5)
    println(s"r4 is $r4")

    val r5 = MonoidLaw.zeroLaw(4)
    println(s"r5 is $r5")

    val r6 = MonoidInstances.fold(Option(33))
    println(s"r6 is $r6")



  }

}
