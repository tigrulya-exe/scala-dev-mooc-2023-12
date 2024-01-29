package module1.homework


import scala.util.Random

class BallsExperiment(val balls: List[Byte] = List(0, 0, 0, 1, 1, 1)) {

  private def getBall(idx: Int, ignoreIdx: Int): Byte = {
    balls.zipWithIndex
      .filter { case (_, idx) => idx != ignoreIdx }(idx)._1
  }

  def isFirstBlackSecondWhite(): Boolean = {
    val firstBallIdx = Random.nextInt(balls.length)
    val secondBallIdx = Random.nextInt(balls.length - 1)
    balls(firstBallIdx) == 0 && getBall(secondBallIdx, firstBallIdx) == 1
  }
}


object BallsTest {
  def main(args: Array[String]): Unit = {
    val count = 1000000
    val listOfExperiments: List[BallsExperiment] = List.fill(count) {
      new BallsExperiment()
    }
    val countOfExperiments = listOfExperiments.map(_.isFirstBlackSecondWhite())
    val countOfPositiveExperiments: Float = countOfExperiments.count(_ == true)
    println(countOfPositiveExperiments / count)
  }
}