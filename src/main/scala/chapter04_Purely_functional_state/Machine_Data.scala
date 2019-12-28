package chapter04_Purely_functional_state

object Machine_Data {

  def main(args: Array[String]): Unit = {

    // 函数式转台变化的一种抽象的逻辑
    case class State[S, +A](run: S => (A, S)) {

      def map[B](f: A => B): State[S, B] = State { s =>
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


      def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
        val (a, s2) = run(s)
        val state = f(a)
        state.run(s2)
      }
    }

    case class Machine(locked: Boolean, remain: Int)


    object Machine {

      def charge: State[Machine, Int] = State {
        case Machine(true, remain) if remain > 0 => (0, Machine(locked = false, remain))
        case x => (0, x)

      }

      def deliver: State[Machine, Int] = State {
        case Machine(false, remain) if remain > 0 => (1, Machine(locked = true, remain - 1))
        case x => (0, x)
      }

      def simulate: State[Machine, Int] = for {
        _ <- charge
        x <- deliver
        _ <- charge
        _ <- charge
        y <- deliver
        _ <- charge
        z <- deliver
        z2 <- deliver
      } yield x + y + z

    }

    val m = Machine.simulate
    println(s"m is ${m.run(Machine(locked = true, 15))}")

  }

}
