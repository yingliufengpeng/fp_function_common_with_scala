package chapter04_Purely_functional_state

import sun.security.provider.NativePRNG

import scala.runtime.Nothing$

object State_Data {


  def main(args: Array[String]): Unit = {

    // 函数式转台变化的一种抽象的逻辑
    case class State[S, +A](run: S => (A, S)) {

      def map[B](f: A => B): State[S, B] = State{ s =>
        val (a, s2) = run(s)
        (f(a), s2)
      }

      def getS: State[S, A] = State { s =>
        run(s)
      }

      def setS(s0: S): State[S, A] = State { s =>
        val (a, _) = run(s)
        (a, s0)
      }


      def flatMap[B](f: A => State[S, B]): State[S, B] = State{ s  =>
        val (a, s2) = run(s)
        val state = f(a)
        state.run(s2)
      }
    }

    object lcg {
      def apply(seed: Long): Long = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    }

    sealed trait RNG {
      def nextInt: (Int, RNG) // 返回新的状态的实例,纯的函数的逻辑
    }

    object rng {
      def apply(seed: Long): RNG = SimpleRng(seed)
    }

    case class SimpleRng(seed: Long) extends RNG {
      override def nextInt: (Int, RNG) = {
        val l = lcg(seed)
        ((l >>> 16).asInstanceOf[Int], SimpleRng(l))
      }
    }

    object rand {

      type Rand[A] = State[RNG, A]

      case class Member(name: String, age: Int)

      def int: Rand[Int] = State(_.nextInt)

      def double: Rand[Double] = int.map(_.toDouble)

      def string: Rand[String] = int.map { e =>
        val names = List("wangpeng", "wuqiang", "bingbing", "gou")
        val index = math.abs(e) % 4
        names(index)
      }

      def age: Rand[Int] = int.map { e =>
        val list = (18 to 28).toList
        val index = math.abs(e) % list.length
        list(index)
      }

      def youngerMember: Rand[Member] = for {
        n <- string
        ag <- age
      } yield Member(n, ag)


    }

    val rng2 = rng(3)
    val (m1, rng3) = rand.youngerMember.run(rng2)
    println(s"m is $m1")

    val (m2, rng4) = rand.youngerMember.run(rng3)
    println(s"m is $m2")

  }
}
