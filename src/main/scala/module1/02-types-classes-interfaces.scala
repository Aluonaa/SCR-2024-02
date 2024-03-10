package module1

import java.io.{Closeable, File}
import scala.io.{BufferedSource, Source}




object type_system {

  /**
   * Scala type system
   *
   */



  def absurd(v: Nothing) = ???


  // Generics


//  val file: File = new File("ints.txt")
//  val source: BufferedSource = Source.fromFile(file)
//
//
//
//  val lines: List[String] = try{
//    source.getLines().toList
//  } finally {
//    source.close()
//  }

  def ensureClose(source: BufferedSource, f: BufferedSource => List[String]): List[String] = try{
    f(source)
  } finally {
    source.close()
  }

  def ensureClose2[S <: {def close(): Unit}, R](source: S)(f: S => R): R = try{
    f(source)
  } finally {
    source.close()
  }


//  val lines2: List[String] = ensureClose(source, s => {
//    s.getLines().toList
//  })
//
//  val lines3: List[String] = ensureClose2(source){s =>
//    s.getLines().toList
//  }





  // ограничения связанные с дженериками


  /**
   *
   * class
   *
   * конструкторы / поля / методы / компаньоны
   */


   class User private(val email: String = "", val password: String = "qwerty"){
     def getEmail(): String = email
     def getPassword(): String = password
   }

   object User{
     def apply(email: String, password: String): User = new User(password = password)
     def from(email: String, password: String): User =
       new User(password = password)
     def from(email: String): User =
       new User(email)
   }

   val user: User = User.from("email@dot.ru")

   val user2: User = User.from("email@dot.ru", "asdf")

   val user3: User = User("email@dot.ru", "asdf")








  /**
   * Задание 1: Создать класс "Прямоугольник"(Rectangle),
   * мы должны иметь возможность создавать прямоугольник с заданной
   * длиной(length) и шириной(width), а также вычислять его периметр и площадь
   *
   */


  /**
   * object
   *
   * 1. Паттерн одиночка
   * 2. Ленивая инициализация
   * 3. Могут быть компаньоны
   */


  /**
   * case class
   *
   */



    // создать case класс кредитная карта с двумя полями номер и cvc


   case class CreditCard(number: Int, cvc: Int)

   val cc: CreditCard = CreditCard(12, 12)

   val cc2: CreditCard = cc.copy(cvc = 14)

   case object Foo



  /**
   * case object
   *
   * Используются для создания перечислений или же в качестве сообщений для Акторов
   */



  /**
   * trait
   *
   */


    trait UserService{
       def get(id: String): User
       def insert(user: User): Unit
    }

    trait WithId{
      def id: String
    }

   class Foo

    class UserServiceImpl(val id: String) extends Foo with UserService {
      def get(id: String): User =  ???

      def insert(user: User): Unit = ???
    }

    val userService = new UserServiceImpl("ab") with WithId













  class A {
    def foo() = "A"
  }

  trait B extends A {
    override def foo() = "B" + super.foo()
  }

  trait C extends B {
    override def foo() = "C" + super.foo()
  }

  trait D extends A {
    override def foo() = "D" + super.foo()
  }

  trait E extends C {
    override def foo(): String = "E" + super.foo()
  }



  // CBDA
  //  A -> D -> B -> C
  // CBDA
  val v = new A with D with C with B


  // A -> B -> C -> E -> D
  val v1 = new A with E with D with C with B


  /**
   * Value classes и Universal traits
   */


}