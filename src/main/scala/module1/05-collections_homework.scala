import scala.util.Random

class Experiment {
  private val ballsUrn: List[Int] = List(1, 0, 1, 0, 1, 0)
}

object Experiment{
  /** Методу подается количество урн, содержащих по 6 шаров **/
  def startChecking(urnsCount: Int): Unit = {
    val result: Double = (0 to 10000).map(_ => getRandomBalls(new Experiment().ballsUrn)).toList.count(v => v)
    println(s"Result is ${result / urnsCount}")
  }

  private def getRandomBalls(ballsUrn: List[Int]): Boolean ={
    val first = Random.nextInt(ballsUrn.size)
    /** Формируется новый List без извлеченного первого шара **/
    val ballsUrnWithoutFirstBall = ballsUrn.zipWithIndex.filter(_._2 != first).map(_._1)
    val second = Random.nextInt(ballsUrnWithoutFirstBall.size)
    if(ballsUrn(first) == 1 || ballsUrnWithoutFirstBall(second) == 1) true
    else false
  }
}

object Test extends App{
  Experiment.startChecking(10000)
}
