package chapter05_Purely_functional_parallelism
import scala.language.postfixOps
import java.util.concurrent.ExecutorService



import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.higherKinds
import scala.util.{Failure, Success}

object parallelism {


  def main(args: Array[String]): Unit = {

    object Demo {
      def sum_0(ints: IndexedSeq[Int]): Int = {
        if (ints.size <= 1) ints.headOption.getOrElse(0)
        else {
          val (l, r) = ints.splitAt(ints.size / 2)
          sum_0(l) + sum_0(r)
        }
      }

      import par._

      //      def sum_2(ints: IndexedSeq[Int]): Int = {
      //        if (ints.size <= 1) ints.headOption.getOrElse(0)
      //        else {
      //          val (l, r) = ints.splitAt(ints.size / 2)
      //          val pl = Par.unit(sum_2(l))
      //          val pr = Par.unit(sum_2(r))
      //          Par.get(pl) + Par.get(pr)
      //        }
      //      }

      import ParImplicits._

      def sum_3(ints: => IndexedSeq[Long]): Par[Long] = {
        if (ints.size <= 1) ints.headOption.getOrElse(0L).unit
        else {
          val (l, r) = ints.splitAt(ints.size / 2)
          val pl = sum_3(l).fork
          val pr = sum_3(r).fork
          pl.map2(pr)(_ + _)
        }
      }
    }

    object par {
      type Par[A] = ExecutionContext => Future[A]

      object Par {

        // 使用lazy的求值的模式的思考
        def unit[A](a: A): Par[A] = _ => Future.successful(a)

        def get[A](a: Par[A]): Future[A] = run(a)

        def map[A, B](fa: Par[A])(f: A => B): Par[B] = map2(fa)(unit(()))((a, _) => f(a))

        def join[A](fa: Par[Par[A]]): Par[A] = e => {
          fa(e).map(_(e)).flatten
        }

        def flatten[A](fa: Par[Par[A]]): Par[A] = join(fa)

        def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = join(map(fa)(f))

        def map2[A, B, C](fa: Par[A])(fb: Par[B])(f: (A, B) => C): Par[C] = e => {
          val future_a = fa(e)
          val future_b = fb(e)
          for {
            a <- future_a
            b <- future_b
          } yield f(a, b)
        }

        def fork[A](a: => Par[A]): Par[A] = e => Future(a(e)).flatten

        def lazyUnit[A](a: => A): par.Par[A] = fork(unit(a))

        def run[A](a: Par[A]): Future[A] = Future {
          val g = implicitly[ExecutionContext]
          a(g)
        }.flatten

        def lift2[A, B](f: A => B): A => Par[B] = a => { _ =>
           Future(f(a))
        }

        // 这个版本好,我很喜欢需要自己继续的思考
        def lift[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))
      }

      object ParImplicits {

        implicit class Function2List[A, B](f: A => B) {
          def lift: A => par.Par[B] = Par.lift(f)
        }

        implicit class Elem2Monad[A](a: A) {
          def unit: par.Par[A] = Par.unit(a)
        }

        implicit class LazyPar2Monad[A](a: => Par[A]) {
          def fork: par.Par[A] = Par.fork(a)

          def lazyUnit: par.Par[par.Par[A]] = Par.lazyUnit(a)
        }

        implicit class Par2Monad[A](a: Par[A]) {
          def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = Par.map2(a)(b)(f)

          def map[B](f: A => B): Par[B] = Par.map(a)(f)

          def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(a)(f)

          def run: Future[A] = Par.run(a)

          def get: Future[A] = run
        }

      }

    }

    import par.ParImplicits._
//    val r = Demo.sum_3((1L to 1000000L))
//    println(s"r is ${Await.result(r.run, 1.hour)}")

    val urls = List(
      "https://www.baidu.com",
      "https://dev.libreneitor.com/all-scala-cats-symbols-operators/",
      "https://cloud.tencent.com/developer/ask/98431",
    )

    def getUrl(url: String): String = {
      import java.net._
      import java.io._
      val url2 = new URL(url)
      val conn = url2.openConnection()
      val reader: BufferedReader = new BufferedReader(new InputStreamReader(conn.getInputStream))

      @scala.annotation.tailrec
      def go(reader: BufferedReader, content: StringBuffer): StringBuffer = {
        val line = reader.readLine()
        if (line == null)
          content
        else go(reader, content.append(line))
      }

      val m = go(reader, new StringBuffer())
      m.toString
    }

    val getUrl2 = (getUrl _) lift

    val m: par.Par[Map[Int, String]] = for {
      x <- getUrl2(urls(0))
      y <- getUrl2(urls(1))
      z <- getUrl2(urls(2))
    } yield Map(0 -> x, 1 -> y, 2 -> z)

    println(s"r is ${Await.result(m.run, 1.hour)}")

  }
}
