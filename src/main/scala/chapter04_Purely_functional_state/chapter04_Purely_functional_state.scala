package chapter04_Purely_functional_state

import java.lang.management.MemoryManagerMXBean

object chapter04_Purely_functional_state {


  def main(args: Array[String]): Unit = {

    object lcg {
      def apply(seed: Long): Long = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    }

    sealed trait RNG {
      def nextInt: (Int, RNG) // 返回新的状态的实例,纯的函数的逻辑
    }

    object RNG {
      def apply(seed: Long) = SimpleRng(seed)

      def nextDouble(rng: RNG): (Double, RNG) = {
        val (i, rng2) = rng.nextInt
        (i.toDouble / Double.MaxValue, rng2)
      }


      val chars = ('a' to 'z').toList ++ ('A' to 'Z').toList

      def nextAlphabet(rng: RNG): (Char, RNG) = {
        val (v, rng2) = rng.nextInt
        val ch = chars(v % chars.length)
        (ch, rng2)
      }
    }

    case class SimpleRng(seed: Long) extends RNG {
      override def nextInt: (Int, RNG) = {
        val l = lcg(seed)
        ((l >>> 16).asInstanceOf[Int], SimpleRng(l))
      }
    }

    val r = RNG(1)
    println(s"r'next is ${r.nextInt}")
    println(s"r'next is ${r.nextInt}")

    val r2 = RNG.nextDouble(r)
    println(s"r2 is $r2")

    object rand {

      type Rand[A] = RNG => (A, RNG)

      case class Member(name: String, age: Int)

      object Rand {

        def int: Rand[Int] = _.nextInt

        def map[A, B](frand: Rand[A])(f: A => B): Rand[B] = { rng =>
          val (l, rng2) = frand(rng)
          (f(l), rng2)
        }

        def flatMap[A, B](frand: Rand[A])(f: A => Rand[B]): Rand[B] = { rng =>
          val (l, rnga) = frand(rng)
          val frandb = f(l)
          frandb(rnga)
        }

        def double: Rand[Double] = map(int)(_.toDouble / Int.MaxValue.toDouble)

        def string: Rand[String] = map(int) { e =>
          val names = List("wangpeng", "wuqiang", "bingbing", "gou")
          val index = math.abs(e) % 4
          names(index)
        }

        def int2: Rand[Int] = map(int) { e =>
          val list = (18 to 28).toList
          val index = math.abs(e) % list.length
          list(index)
        }


        import RandImplicits._

        def younerMember: Rand[Member] = for {
          name <- string
          age <- int2
        } yield Member(name, age)

      }

      object RandImplicits {

        implicit class Rand2Monad[A](a: Rand[A]) {
          def map[B](f: A => B): Rand[B] = Rand.map(a)(f)

          def flatMap[B](f: A => Rand[B]): Rand[B] = Rand.flatMap(a)(f)
        }

      }

    }

    val r3 = rand.Rand.double
    println(s"r3' next is ${r3(r)}")

    val rng = rand.Rand.flatMap(rand.Rand.int)(e => {
      rng =>
        val (i, rng2) = rng.nextInt
        ((i + e).toString, rng2)
    })


    val youngmeber = rand.Rand.younerMember

    val (m1, rng2) = youngmeber(RNG(22))
    println(s"m is $m1")

    val (m2, rng3) = youngmeber(rng2)
    println(s"m2 is $m2")


  }
}
