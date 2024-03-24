package module3

import module3.zioConcurrency.printEffectRunningTime
import module3.zio_homework.config.AppConfig
import zio.clock.Clock
import zio.console.{Console, getStrLn, putStrLn}
import zio.duration.durationInt
import zio.random.Random
import zio.{Has, URIO, ZIO, ZLayer}

import java.util.concurrent.TimeUnit
import scala.language.postfixOps

object Main {

  def main(args: Array[String]): Unit = {
    var iter = 3

    val effect = for {
      _ <- putStrLn(s"Current val: $iter")
      _ <- ZIO.effect(iter -= 1)
      needBreak <- ZIO.succeed(iter == 0)
    } yield needBreak


    //    zio.Runtime.default.unsafeRun(zio_homework.doWhile(effect))
    //    zio.Runtime.default.unsafeRun(zio_homework.loadConfigOrDefault)
    //    zio.Runtime.default.unsafeRun(zio_homework.app)
    zio.Runtime.default.unsafeRun(zio_homework.appSpeedUp)
  }
}

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */


  lazy val guessProgram: ZIO[Console with Random, Exception, Unit] = for {
    targetNumber: Int <- zio.random.nextIntBetween(1, 3)
    _ <- putStrLn("Print any number between 1 and 3 inclusive:")
    inputNumber <- getStrLn
    _ <- if (inputNumber.toInt != targetNumber)
      putStrLn("Wrong number :(")
    else
      putStrLn("Well done!")
  } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   *
   */

  def doWhile[R, E](iteration: ZIO[R, E, Boolean]): ZIO[R, E, Boolean] = iteration.flatMap(
    result => if (!result) doWhile(iteration) else ZIO.succeed(result)
  )

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */


  def loadConfigOrDefault: URIO[Console, Unit] = for {
    cfg <- config.load.orElseSucceed(AppConfig("test_host", "777"))
    _ <- putStrLn(cfg.toString)
  } yield ()


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: URIO[Clock with Random with Console, Int] = for {
    _ <- ZIO.sleep(1 seconds)
    randomNum <- zio.random.nextIntBetween(1, 10)
    _ <- putStrLn(s"Random: $randomNum")
  } yield randomNum

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects = Seq.fill(10)(eff)


  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекции "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */


  def printEffectRunningTimeWithResult[R, E, A](effect: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A] = for {
    result <- printEffectRunningTime(effect)
    _ <- putStrLn(s"Result: $result")
  } yield result

  val sumOfRandoms: URIO[Console with Clock with Random, Int] = effects.reduce {
    _.zipWith(_)(_ + _)
  }

  lazy val app = printEffectRunningTimeWithResult(sumOfRandoms)


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  val sumOfRandomsPar: URIO[Console with Clock with Random, Int] = effects.reduce {
    _.zipWithPar(_)(_ + _)
  }

  lazy val appSpeedUp = printEffectRunningTimeWithResult(sumOfRandomsPar)


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * можно было использовать аналогично zio.console.putStrLn например
   */

  type RunningTimePrinter = Has[RunningTimePrinter.Service]

  object RunningTimePrinter {
    trait Service {
      def printRunningTime[R, E, A](effect: ZIO[R, E, A]): ZIO[RunningTimePrinter with R, E, A]
    }

    case class ServiceImpl(console: Console.Service, clock: Clock.Service) extends Service {

      override def printRunningTime[R, E, A](effect: ZIO[R, E, A]): ZIO[RunningTimePrinter with R, E, A] = for {
        start <- clock.currentTime(TimeUnit.SECONDS)
        r <- effect
        end <- clock.currentTime(TimeUnit.SECONDS)
        _ <- console.putStrLn(s"Running time ${end - start}")
      } yield r
    }

    val live: ZLayer[Console with Clock, Nothing, RunningTimePrinter] = ZLayer.fromServices[Console.Service, Clock.Service, RunningTimePrinter.Service](
      (consoleService, clockService) => ServiceImpl(consoleService, clockService)
    )

    def printRunningTime[R, E, A](effect: ZIO[R, E, A]): ZIO[RunningTimePrinter with R, E, A] = {
      ZIO.accessM(_.get.printRunningTime(effect))
    }
  }

  /**
   * 6.
   * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
   *
   *
   */

  lazy val appWithTimeLogg: ZIO[RunningTimePrinter with Console with Clock with Random, Nothing, Int] =
    RunningTimePrinter.printRunningTime(sumOfRandoms)

  /**
   *
   * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
   */

  lazy val runApp: ZIO[Console with Clock with Random, Nothing, Int] = appWithTimeLogg.provideSomeLayer[Console with Clock with Random](RunningTimePrinter.live)

}
