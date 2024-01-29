package collections

object task_collections {

  def isASCIIString(str: String): Boolean = str.matches("[A-Za-z]+")

  /**
   * Реализуйте метод который первый элемент списка не изменяет, а для последующих алгоритм следующий:
   * если isASCIIString is TRUE тогда пусть каждый элемент строки будет в ВЕРХНЕМ регистре
   * если isASCIIString is FALSE тогда пусть каждый элемент строки будет в нижнем регистре
   * Пример:
   * capitalizeIgnoringASCII(List("Lorem", "ipsum" ,"dolor", "sit", "amet")) -> List("Lorem", "IPSUM", "DOLOR", "SIT", "AMET")
   * capitalizeIgnoringASCII(List("Оказывается", "," "ЗвУк", "КЛАВИШЬ", "печатной", "Машинки", "не", "СТАЛ", "ограничивающим", "фактором")) ->
   * List("Оказывается", "," "звук", "КЛАВИШЬ", "печатной", "машинки", "не", "стал", "ограничивающим", "фактором")
   * HINT: Тут удобно использовать collect и zipWithIndex
   *
   * * */
  def capitalizeIgnoringASCII(text: List[String]): List[String] = {
    text.zipWithIndex
      .collect {
        case (word, idx) if idx == 0 => word
        case (word, _) if isASCIIString(word) => word.toUpperCase()
        case (word, _) => word.toLowerCase()
      }
  }

  val actualCapitalizedWords = capitalizeIgnoringASCII(List("Lorem", "ipsum" ,"dolor", "sit", "amet"))
  val expectedCapitalizedWords = List("Lorem", "IPSUM", "DOLOR", "SIT", "AMET")
  assert(expectedCapitalizedWords == actualCapitalizedWords)

  val actualCapitalizedWords2 = capitalizeIgnoringASCII(List("Оказывается", ",", "ЗвУк", "КЛАВИШЬ", "печатной",
    "Машинки", "не", "СТАЛ", "ограничивающим", "фактором"))
  val expectedCapitalizedWords2 = List("Оказывается", ",", "звук", "клавишь", "печатной", "машинки", "не",
    "стал", "ограничивающим", "фактором")
  assert(actualCapitalizedWords2 == expectedCapitalizedWords2)

  /**
   *
   * Компьютер сгенерировал текст используя вместо прописных чисел, числа в виде цифр, помогите компьютеру заменить цифры на числа
   * В тексте встречаются числа от 0 до 9
   *
   * Реализуйте метод который цифровые значения в строке заменяет на числа: 1 -> one, 2 -> two
   *
   * HINT: Для всех возможных комбинаций чисел стоит использовать Map
   * * */
  private val DIGITS_TO_STR = Map(
    '0' -> "zero",
    '1' -> "one",
    '2' -> "two",
    '3' -> "three",
    '4' -> "four",
    '5' -> "five",
    '6' -> "six",
    '7' -> "seven",
    '8' -> "eight",
    '9' -> "nine"
  )

  def numbersToNumericString(text: String): String = {
    text.flatMap {
      case char if DIGITS_TO_STR.contains(char) => DIGITS_TO_STR(char)
      case char => char.toString
    }
  }

  val actualTextWithNumericDigits = numbersToNumericString("test 1 2 4,  7, hello! My Finland 4.")
  val expectedTextWithNumericDigits = "test one two four,  seven, hello! My Finland four."
  assert(actualTextWithNumericDigits == expectedTextWithNumericDigits)

  val actualText = numbersToNumericString("hello, world!")
  val expectedText = "hello, world!"
  assert(actualText == expectedText)

  /**
   *
   * У нас есть два дилера со списками машин которые они обслуживают и продают (case class Auto(mark: String, model: String)).
   * Базы данных дилеров содержат тысячи и больше записей. Нет гарантии что записи уникальные и не имеют повторений
   * HINT: Set
   * HINT2: Iterable стоит изменить
   * * */

  case class Auto(mark: String, model: String)

  /**
   * Хотим узнать какие машины можно обслужить учитывая этих двух дилеров
   * Реализуйте метод который примет две коллекции (два источника) и вернёт объединенный список уникальный значений
   * */
  def intersectionAuto(dealerOne: Iterable[Auto], dealerTwo: Iterable[Auto]): Iterable[Auto] = {
    val secondAutos = dealerTwo.toSet
    dealerOne
      .filter(secondAutos.contains)
      .toSet
  }

  val dealerOne = Seq(Auto("VW", "Passat"), Auto("Porsche", "Cayene"))
  val dealerTwo = Seq(Auto("VW", "Passat"), Auto("Toyota", "Prius"))

  val actualIntersection = intersectionAuto(dealerOne, dealerTwo)
  val expectedIntersection = Set(Auto("VW", "Passat"))
  assert(actualIntersection == expectedIntersection)

  /**
   * Хотим узнать какие машины обслуживается в первом дилеромском центре, но не обслуживаются во втором
   * Реализуйте метод который примет две коллекции (два источника)
   * и вернёт уникальный список машин обслуживающихся в первом дилерском центре и не обслуживающимся во втором
   * */
  def filterAllLeftDealerAutoWithoutRight(dealerOne: Iterable[Auto], dealerTwo: Iterable[Auto]): Iterable[Auto] = {
    val secondAutos = dealerTwo.toSet
    dealerOne
      .filterNot(secondAutos.contains)
      .toSet
  }

  val actualDiff = filterAllLeftDealerAutoWithoutRight(dealerOne, dealerTwo)
  val expectedDiff = Set(Auto("Porsche", "Cayene"))
  assert(actualDiff == expectedDiff)
}