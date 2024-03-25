package module3.zio_homework

import module3.zio_homework
import org.scalatest.flatspec.AnyFlatSpec
import zio.ZIO
import zio.console.putStrLn

class ZioHomeworkTest extends AnyFlatSpec {
  it should "run guess" in {
    zio.Runtime.default.unsafeRun(zio_homework.guessProgram)
  }

  it should "run do while" in {
    var iter = 3
    val iterationEffect = for {
      _ <- putStrLn(s"Current val: $iter")
      _ <- ZIO.effect(iter -= 1)
      needBreak <- ZIO.succeed(iter == 0)
    } yield needBreak

    zio.Runtime.default.unsafeRun(zio_homework.doWhile(iterationEffect))

    assert(iter == 0)
  }

  it should "run load config" in {
    zio.Runtime.default.unsafeRun(zio_homework.loadConfigOrDefault)
  }

  it should "run simple random num sum" in {
    zio.Runtime.default.unsafeRun(zio_homework.app)
  }

  it should "run parallel random num sum" in {
    zio.Runtime.default.unsafeRun(zio_homework.appSpeedUp)
  }

  it should "run time logging app" in {
    val app = ZioHomeWorkApp.run(List())
    zio.Runtime.default.unsafeRun(app)
  }
}
