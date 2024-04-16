package catsconcurrency.cats_effect_homework

import cats.effect.{IO, IOApp}
import cats.implicits._

import scala.concurrent.duration.DurationLong

// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени. Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков, 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())
object WalletFibersApp extends IOApp.Simple {

  def run: IO[Unit] =
    for {
      _ <- IO.println("Press any key to stop...")
      wallet1 <- Wallet.fileWallet[IO]("1")
      wallet2 <- Wallet.fileWallet[IO]("2")
      wallet3 <- Wallet.fileWallet[IO]("3")
      // todo: запустить все файберы и ждать ввода от пользователя чтобы завершить работу
      worker1 <- worker(wallet1, 100).start
      worker2 <- worker(wallet2, 500).start
      worker3 <- worker(wallet3, 2000).start

      observer <- walletsObserver(1000, wallet1, wallet2, wallet3).start

      _ <- IO.readLine

      _ <- worker1.cancel
      _ <- worker2.cancel
      _ <- worker3.cancel
      _ <- observer.cancel
    } yield ()

  private def walletsObserver(showDelay: Long, wallets: Wallet[IO]*): IO[Unit] =
    walletsObserverIteration(showDelay, wallets: _*).foreverM

  private def walletsObserverIteration(showDelay: Long, wallets: Wallet[IO]*): IO[Unit] = {
    for {
      _ <- IO.sleep(showDelay.milliseconds)
      _ <- wallets.map(walletObserver).sequence_
      _ <- IO.println("====================")
    } yield ()
  }

  private def walletObserver(wallet: Wallet[IO]): IO[Unit] = {
    for {
      balance <- wallet.balance
      _ <- IO.println(s"Balance for wallet ${wallet.id}: $balance")
    } yield ()
  }

  private def worker(wallet: Wallet[IO], writeDelay: Long): IO[Unit] =
    workerIteration(wallet, writeDelay).foreverM

  private def workerIteration(wallet: Wallet[IO], writeDelay: Long): IO[Unit] = {
    for {
      _ <- IO.sleep(writeDelay.milliseconds)
      result <- wallet.topup(100)
    } yield result
  }
}
