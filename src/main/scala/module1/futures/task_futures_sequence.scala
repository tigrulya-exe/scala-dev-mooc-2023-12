package module1.futures
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    val acc = Future.successful((List[A](), List[Throwable]()))

    futures.foldRight(acc) { (future, currentResultsFuture) =>
      currentResultsFuture.flatMap { currentResults =>
        future.transform {
          case Success(value) =>
            Success((value :: currentResults._1, currentResults._2))
          case Failure(exception) =>
            Success((currentResults._1, exception :: currentResults._2))
        }
      }
    }
  }

}
