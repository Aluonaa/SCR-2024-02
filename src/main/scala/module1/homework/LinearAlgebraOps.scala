package module1.homework

object LinearAlgebraOps{
  def sum(v1: Array[Int], v2: Array[Int]): Array[Int] = {
    if (v1.length == v2.length) equalLengthSum(v1, v2)
    else throw new Exception("Operation is not supported")
  }

  def scale(a: Int, v: Array[Int]): Array[Int] = {
    v.map(v => v * a)
  }

  def axpy(a: Int, v1: Array[Int], v2: Array[Int]): Array[Int] = {
    if (v1.length == v2.length) equalLengthSum(v1.map(v1 => v1 * a), v2)
    else throw new Exception("Operation is not supported")
  }

  private def equalLengthSum(v1: Array[Int], v2: Array[Int]): Array[Int] = {
    v1.zip(v2).map(v => v._1 + v._2)
  }
}