package module1

import java.time.LocalDate
import java.time.YearMonth

object adt {

  object tuples {

    /** Tuples ()
     *
     *
      */

    object Foo
    type ProductFooBoolean = (Foo.type, Boolean)


    /** Создать возможные экземпляры с типом ProductUnitBoolean
      */

    lazy val p1: ProductFooBoolean = (Foo, true)
    lazy val p2: ProductFooBoolean = (Foo, false)


    /** Реализовать тип Person который будет содержать имя и возраст
      */

    type Person = (String, Int)

    val p: Person = ("Tony", 40)


    /**  Реализовать тип `CreditCard` который может содержать номер (String),
      *  дату окончания (java.time.YearMonth), имя (String), код безопастности (Short)
      */

    type CreditCard = (String, java.time.YearMonth, String, Short)
    val cc = ("12567866", java.time.YearMonth.now(), "Alex", 123)


  }

  object case_classes {

    /** Case classes
      */



    //  Реализовать Person с помощью case класса

    case class Person(name: String, age: Int)

    // Создать экземпляр для Tony Stark 42 года

    val p: Person = Person("Tony", 42)


    // Создать case class для кредитной карты
    case class CreditCard(number: String, exp: java.time.YearMonth, name: String, cvc: Short)
    val cc = CreditCard("12567866", java.time.YearMonth.now(), "Alex", 123)

  }



  object either {

    /** Sum
      */

    /** Either - это наиболее общий способ хранить один из двух или более кусочков информации в одно время.
      * Также как и кортежи обладает целым рядом полезных методов
      * Иммутабелен
      */


    object Bar

    type BooleanOrBar = Either[Boolean, Bar.type]

    val o1: BooleanOrBar = Left(true)
    val o2: BooleanOrBar = Left(false)
    val o3: BooleanOrBar = Right(Bar)

    type IntOrString = Either[Int, String]

    val o4: IntOrString = Left(5)
    val o5: IntOrString = Right("foo")

    /** Реализовать экземпляр типа IntOrString с помощью конструктора Right
      */



    type CreditCard
    type WireTransfer
    type Cash

    /** \
      * Реализовать тип PaymentMethod который может быть представлен одной из альтернатив
      */
    type PaymentMethod = Either[CreditCard, Either[WireTransfer, Cash]]

  }

  object sealed_traits {

    /** Также Sum type можно представить в виде sealed trait с набором альтернатив
      */

    sealed trait PaymentMethod
    final case object CreditCard extends PaymentMethod
    final case object WireTransfer extends PaymentMethod
    final case object Cash extends PaymentMethod

    val pm1: PaymentMethod = CreditCard
    val pm2: PaymentMethod = WireTransfer
    val pm4: PaymentMethod = Cash



  }

  object cards {

    type  Suit                    // масть
    type  Clubs                   // крести
    type  Diamonds                // бубны
    type  Spades                  // пики
    type  Hearts                  // червы
    type  Rank                    // номинал
    type  Two                     // двойка
    type  Three                   // тройка
    type  Four                    // четверка
    type  Five                    // пятерка
    type  Six                     // шестерка
    type  Seven                   // семерка
    type  Eight                   // восьмерка
    type  Nine                    // девятка
    type  Ten                     // десятка
    type  Jack                    // валет
    type  Queen                   // дама
    type  King                    // король
    type  Ace                     // туз
    type  Card                    // карта
    type  Deck                    // колода
    type  Hand                    // рука
    type  Player                  // игрок
    type  Game                    // игра
    type  PickupCard              // взять карту

  }

}
