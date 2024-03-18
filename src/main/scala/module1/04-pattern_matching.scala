package module1

object pattern_matching{
     // Pattern matching

  /**
   * Матчинг на типы
   */

   val i: Any = ???

   i match {
     case v: Int => println("Int")
     case v: String => println("String")
     case v: List[String] => println("List[String]")
     case v: List[Int] => println("List[Int]")
   }




  /**
   * Структурный матчинг
   */




  sealed trait Animal{

    def whoIam = this match {
      case Dog(n, _) => println(s"I'm dog $n")
      case Cat(_, a) => println(s"I'm cat $a")
    }

    def whoIam2 = this match {
      case d: Dog=> println(s"I'm dog ${d.name}")
      case c: Cat => println(s"I'm cat ${c.name}")
    }

  }


  case class Dog(name: String, age: Int) extends Animal
  case class Cat(name: String, age: Int) extends Animal


  /**
   * Матчинг на литерал
   */

  val animal: Animal = ???


  val Bim = "Bim"

  animal match {
    case Dog("Bim", age) => println("I'm dog - Bim")
    case Cat(name, age) => println(s"I'm cat $name")
    case _ => println("Smth else")
  }


  /**
   * Матчинг на константу
   */

  animal match {
    case Dog(Bim, age) => println("I'm dog - Bim")
    case Cat(name, age) => println(s"I'm cat $name")
    case _ => println("Smth else")
  }




  /**
   * Матчинг с условием (гарды)
   */

  animal match {
    case Dog(name, age) => ???
    case Cat(_, age) if age > 5 => println("I'm cat and old enough")
    case Cat(name, _) => println(s"I'm cat $name")
  }


  /**
   * "as" паттерн
   */

  def treatCat(cat: Cat) = ???
  def treatDog(dog: Dog) = ???



  /**
   * используя паттерн матчинг напечатать имя и возраст
   */

  def treatAnimal(a: Animal) = a match {
    case d @ Dog(name, age) =>
      treatDog(d)

    case c @ Cat(name, age) => treatCat(c)
  }


  final case class Employee(name: String, address: Address)
  final case class Address(val street: String, val number: Int)


  val alex = Employee("Alex", Address("Pushkin str", 26))



  /**
   * воспользовавшись паттерн матчингом напечатать номер из поля адрес
   */


   alex match {
     case Employee(_, Address(_, number)) => println(number)
   }


   class Person(val name: String, val age: Int)

   val p = Person("Alex", 42)

   val Person(n, age) = p

   val n2 = p.name
   val age2 = p.age

   object Person{
     def unapply(p: Person): Option[(String, Int)] = ???
     def apply(name: String, age: Int): Person = ???
   }

  p match {
    case Person(n, a) => println(a)
  }



  /**
   * Паттерн матчинг может содержать литералы.
   * Реализовать паттерн матчинг на alex с двумя кейсами.
   * 1. Имя должно соотвествовать Alex
   * 2. Все остальные
   */




  /**
   * Паттерны могут содержать условия. В этом случае case сработает,
   * если и паттерн совпал и условие true.
   * Условия в паттерн матчинге называются гардами.
   */



  /**
   * Реализовать паттерн матчинг на alex с двумя кейсами.
   * 1. Имя должно начинаться с A
   * 2. Все остальные
   */


  /**
   *
   * Мы можем поместить кусок паттерна в переменную использую `as` паттерн,
   * x @ ..., где x это любая переменная.
   * Это переменная может использоваться, как в условии,
   * так и внутри кейса
   */

    trait PaymentMethod
    case object Card extends PaymentMethod
    case object WireTransfer extends PaymentMethod
    case object Cash extends PaymentMethod

    case class Order(paymentMethod: PaymentMethod)

    lazy val order: Order = ???

    lazy val pm: PaymentMethod = ???


    def checkByCard(o: Order) = ???

    def checkOther(o: Order) = ???



  /**
   * Мы можем использовать вертикальную черту `|` для матчинга на альтернативы
   */

   sealed trait A
   case class B(v: Int) extends A
   case class C(v: Int) extends A
   case class D(v: Int) extends A

   val a: A = ???

   a match {
     case B(_) | C(_) => ???
     case D(_) => ???
   }


}