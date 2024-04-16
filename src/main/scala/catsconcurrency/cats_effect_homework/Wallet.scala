package catsconcurrency.cats_effect_homework

import cats.effect.Sync
import cats.implicits._
import catsconcurrency.cats_effect_homework.Wallet._

import java.nio.file.{Files, Path}

// DSL управления электронным кошельком
trait Wallet[F[_]] {
  // возвращает текущий баланс
  def balance: F[BigDecimal]

  // пополняет баланс на указанную сумму
  def topup(amount: BigDecimal): F[Unit]

  // списывает указанную сумму с баланса (ошибка если средств недостаточно)
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]]

  // just for debugging
  def id: WalletId
}

// Игрушечный кошелек который сохраняет свой баланс в файл
// todo: реализовать используя java.nio.file._
// Насчёт безопасного конкуррентного доступа и производительности не заморачиваемся, делаем максимально простую рабочую имплементацию.
// (Подсказка - можно читать и сохранять файл на каждую операцию).
// Важно аккуратно и правильно завернуть в IO все возможные побочные эффекты.
//
// функции которые пригодятся:
// - java.nio.file.Files.write
// - java.nio.file.Files.readString
// - java.nio.file.Files.exists
// - java.nio.file.Paths.get

final class FileWallet[F[_] : Sync](val id: WalletId) extends Wallet[F] {
  private val walletFilePath = Path.of("wallets", id)

  def balance: F[BigDecimal] = {
    for {
      _ <- maybeCreateWalletFile()
      currentBalance <- Sync[F].delay(Files.readString(walletFilePath))
        .map(BigDecimal(_))
    } yield currentBalance
  }

  def topup(amount: BigDecimal): F[Unit] = {
    for {
      currentBalance <- balance
      _ <- writeBalance(currentBalance + amount)
    } yield ()
  }

  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] = {
    for {
      currentBalance <- balance
      result <- if (currentBalance < amount) {
        Sync[F].pure(Left(BalanceTooLow))
      } else {
        writeBalance(currentBalance - amount).map(_.asRight)
      }
    } yield result
  }

  private def maybeCreateWalletFile(): F[Unit] = {
    for {
      fileExists <- Sync[F].delay(Files.exists(walletFilePath))
      _ <- if (!fileExists) {
        createWalletFile()
      } else {
        Sync[F].unit
      }
    } yield ()
  }

  private def createWalletFile(): F[Unit] = {
    for {
      _ <- Sync[F].delay(Files.createDirectories(walletFilePath.getParent))
      _ <- Sync[F].delay(Files.createFile(walletFilePath))
      _ <- writeBalance(BigDecimal(0))
    } yield ()
  }

  private def writeBalance(balance: BigDecimal): F[Unit] = {
    Sync[F].delay(Files.writeString(walletFilePath, balance.toString))
  }
}

object Wallet {

  // todo: реализовать конструктор
  // внимание на сигнатуру результата - инициализация кошелька имеет сайд-эффекты
  // Здесь нужно использовать обобщенную версию уже пройденного вами метода IO.delay,
  // вызывается она так: Sync[F].delay(...)
  // Тайпкласс Sync из cats-effect описывает возможность заворачивания сайд-эффектов
  def fileWallet[F[_] : Sync](id: WalletId): F[Wallet[F]] = {
    Sync[F].delay(new FileWallet(id))
  }

  type WalletId = String

  sealed trait WalletError

  case object BalanceTooLow extends WalletError
}
