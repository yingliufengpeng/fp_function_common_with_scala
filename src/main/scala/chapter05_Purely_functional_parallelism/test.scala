package chapter05_Purely_functional_parallelism

object test {

  object M {

    def mm(m: => List[Int]): Int = {
      println(s"m is ${m.sum}")
      0
    }

    implicit class NN(m: => List[Int]) {

      def nn: Int = mm(m)
    }

  }

  def main(args: Array[String]): Unit = {
    import M._
//    val m = mm({println(s"okkkk");(1 to 10).toList})
//    println(s"m is $m")

//    val n = {println(s"kkkk");(1 to 10).toList}.nn
//    println(s"n is $n")

    val n2 = (1 to 10000000).toList.nn

  }

}
