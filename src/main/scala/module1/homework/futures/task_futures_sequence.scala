package module1.homework.futures


import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

object task_futures_sequence{

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
    val promise = Promise[(List[A], List[Throwable])]

    def loop(futures: List[Future[A]] = futures, success: List[A] = List(), fail: List[Throwable] = List()): Unit = futures match {
      case Nil => promise.success(success.reverse, fail.reverse)
      case ::(head, _) => head.onComplete({
      case Success(v) => loop(futures.tail, v :: success, fail)
      case Failure(e) => loop(futures.tail, success, e :: fail)
      })
    }
    loop()
    promise.future
  }
}
