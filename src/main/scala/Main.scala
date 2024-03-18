import module1.{hof, lazyOps, list, type_system}


object Main {

  def main(args: Array[String]): Unit = {
    println("Hello, World!")
    val l1 = list.List(1, 2, 3)
    val l2 = list.List(4, 5, 6)

    // println(l1.drop(2))
//    val l3: list.List[Int] = for{
//      e1 <- l1
//      e2 <- l2
//    } yield e1 + e2
//
//    val l4: list.List[Int] = l1.flatMap(e1 => l2.map(e2 => e1 + e2))
//
//    val op1: Option[Int] = Some(1)
//    val op2: Option[Int] = Some(2)
//
//    val op3: Option[Int] = for{
//      e1 <- op1
//      e2 <- op2
//    } yield e1 + e2
//
//    op1.flatMap(e1 => op2.map(e2 => e1 + e2))

      lazyOps
  }
}

