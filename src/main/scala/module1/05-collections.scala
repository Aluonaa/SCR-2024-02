package module1

object coll{

  // создать список чисел

  val numbers = List(1, 2, 3, 4, 5)

  // удвоить числа в списке
  val doubleNumbers = numbers.map(_ * 2)


  // преобразовать список слов, в список букв

  val words: List[String] = List("Hello", "World")

  val chars: List[Char] = words.flatMap(_.toList)


  // отфильтровать четные числа из списка numbers

  val evenNumbers: List[Int] = numbers.filter(_ % 2 == 0)


  val mixedList = List("apple", 2, "banana", 4, "cherry", 6)

  // получить список только из чисел

  val numbersOnly = mixedList.collect{case x: Int => x}


  // Посчитать сумму чисел в списке numbers

  val sum = numbers.reduce(_ + _)


  // комбинация элементов с помощью бинарной ассоциативной операции и аккумулятора
  val product = numbers.fold(1)(_ * _)


  // комбинация элементов с помощью бинарной ассоциативной операции с лева направо
  // найти максимальное число

  val max = numbers.reduceLeft((a, b) => if(a > b) a else b)

  // комбинация элементов с помощью бинарной ассоциативной операции с лева направо и аккумулятора


  // комбинация элементов с помощью бинарной ассоциативной операции c сохранением промежуточных
  // результатов
  val sumScan: List[Int] = numbers.scan(0)(_ + _)


  // Map


  // сортировка

  case class User(email: String, password: String)
  val users = List(User("b@yandex.ru", "345"), User("a@yandex.ru", "123"), User("c@yandex.ru", "078"))


  val sortedNumbers = numbers.sortWith((a, b) => a > b)
  val usersSortedByEmail = users.sortBy(_.email)
  // Цепочка операций

  val result1 = numbers.filter(_ % 2 == 0).map(_ * 2).reverse

  val result2 = numbers.distinct.drop(2).take(2).map(_ * 10)


}