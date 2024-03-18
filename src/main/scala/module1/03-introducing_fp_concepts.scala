package module1

import module1.list.List.::

import scala.annotation.tailrec
import scala.language.postfixOps



 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n){
      _n *= i
      i += 1
    }
    _n
  }

  def factRec(n: Int): Int = if(n <= 0) 1 else n * factRec(n - 1)

  def factTailRec(n: Int): Int = {
    @tailrec
    def loop(n: Int, acc: Int): Int = {
      if(n <= 0) acc
      else loop(n - 1, n * acc)
    }
    loop(n, 1)
  }





  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   *
   */


}

object hof{


  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
    val start = System.currentTimeMillis()
    val result: B = f(a)
    val end = System.currentTimeMillis()
    println(s"Running time: ${end - start}")
    result
  }

  def doomy(str: String): Unit = {
    Thread.sleep(1000)
    println(str)
  }



  // изменение поведения ф-ции

  def isOdd(i: Int): Boolean = i % 2 > 0

  def not[A](f: A => Boolean): A => Boolean = a => !f(a)

  lazy val isEven: Int => Boolean = not(isOdd)

  isOdd(2)
  isEven(3)


  // изменение самой функции

  def partial[A, B, C](a: A, f: (A, B) => C): B => C = f.curried(a)

  def sum(x: Int, y: Int): Int = x + y

  val _: Int => Int => Int = sum _.curried
  val p: Int => Int = (sum _.curried)(2)
  p(3) // 5















  trait Consumer{
       def subscribe(topic: String): Stream[Record]
   }

   case class Record(value: String)

   case class Request()

   object Request {
       def parse(str: String): Request = ???
   }

  /**
   *
   * (Опционально) Реализовать ф-цию, которая будет читать записи Request из топика,
   * и сохранять их в базу
   */
   def createRequestSubscription() = ???



}






/**
 *  Реализуем тип Option
 */



 object opt {


  class Animal
  class Dog extends Animal

  /**
   *
   * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутсвие результата
   */

  // 1. invariance
  // 2. covariance  A <- B  Option[A] <- Option[B]
  // 3. contravariance A <- B Option[A] -> Option[B]


  sealed trait Option[+T]{

    def isEmpty: Boolean = this match {
      case Some(v) => false
      case None => true
    }

    def get: T = this match {
      case Some(v) => v
      case None => throw new Exception("get on empty option")
    }


    def map[B](f: T => B): Option[B] = flatMap(t => Option(f(t)))

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Some(v) => f(v)
      case None => None
    }

    /**
     *
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */

    def printIfAny[A](): Unit = if(!this.isEmpty) println(this)

    /**
     *
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */

    def zip[A](v: Option[A]) : Option[(T, A)] = {
      if(!this.isEmpty && !v.isEmpty) Option(this.get, v.get)
      else None
    }

    /**
     *
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true
     */

    def filter(f: T => Boolean): Option[T] = {
      if (!this.isEmpty && f(this.get)) this
      else None
    }
  }

  case class Some[V](v: V) extends Option[V]
  case object None extends Option[Nothing]   // Any <- Dog

  var o11: Option[Int] = None

  object Option{
    def apply[T](v: T): Option[T] =
      if(v == null) None
      else Some(v)
  }

  val o1: Option[Int] = Option(1)
  o1.isEmpty // false


}

 object list {

   /**
    *
    * Реализовать односвязанный иммутабельный список List
    * Список имеет два случая:
    * Nil - пустой список
    * Cons - непустой, содержит первый элемент (голову) и хвост (оставшийся список)
    */


    sealed trait List[+T]{

     /**
      * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
      *
      */

     def ::[A >: T](head: A): List[A] = new ::(head, this)

     /**
      *
      * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
      */


     def map[B](f: T => B): List[B] = {
       @tailrec
       def loop(list: List[T], acc:List[B] = List[B]()): List[B] = {
         list match {
           case ::(head,tail) => loop(tail, f(head)::acc)
           case List.Nil => acc.reverse
         }
       }
       loop(this)
     }

     /**
      * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
      *
      */

     def mkString(delimiter: String): String = this match {
       case ::(head, tail) => s"$head$delimiter" + tail.mkString(delimiter)
       case ::(head, List.Nil) => s"$head"
       case List.Nil => ""
     }

     /**
      *
      * Реализовать метод filter для списка который будет фильтровать список по некому условию
      */

     def filter(f: T => Boolean): List[T] = {
       @tailrec
       def loop(list: List[T], acc: List[T] = List()): List[T] = {
         list match {
           case ::(head,tail) if f(head) => loop(tail, head :: acc)
           case List.Nil => acc
         }
       }
       loop(this).reverse
     }


     /**
      *
      * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
      */

     def reverse: List[T] = {
       @tailrec
       def loop(list: List[T], acc: List[T]): List[T] = {
         list match {
           case ::(head,tail) => loop(tail, head :: acc)
           case List.Nil => acc
         }
       }
       loop(this,List())
     }

     /**
      *
      * Написать функцию shoutString котрая будет принимать список String и возвращать список,
      * где к каждому элементу будет добавлен префикс в виде '!'
      */


     def shoutString(list: List[String]): List[String] = list.map(v => "!" + v)

     /**
      *
      * Написать функцию incList котрая будет принимать список Int и возвращать список,
      * где каждый элемент будет увеличен на 1
      */

     def incList(list: List[Int]): List[Int] = list.map(v => v + 1)

    }

    object List{
      case class ::[A](head: A, tail: List[A]) extends List[A]
      case object Nil extends List[Nothing]

      def apply[A](v: A*): List[A] =
        if(v.isEmpty) List.Nil else ::(v.head, apply(v.tail:_*))
    }
 }