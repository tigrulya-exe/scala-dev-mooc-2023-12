package module2

import module2.type_classes.JsValue.{JsNull, JsNumber, JsString}


object type_classes {

  sealed trait JsValue
  object JsValue {
    final case class JsObject(get: Map[String, JsValue]) extends JsValue
    final case class JsString(get: String) extends JsValue
    final case class JsNumber(get: Double) extends JsValue
    final case object JsNull extends JsValue
  }

  // 1
  trait JsonWriter[T]{
    def write(v: T): JsValue
  }

  object JsonWriter{

    def apply[T](implicit ev: JsonWriter[T]): JsonWriter[T] = ev

    def from[T](f: T => JsValue) = new JsonWriter[T] {
      override def write(v: T): JsValue = f(v)
    }

    // 2
    implicit val strToJsonWriter = from[String](JsString)

    implicit val intToJsonWriter = from[Int](JsNumber(_))

    implicit def optToJsonWriter[T](implicit ev: JsonWriter[T]) =
      from[Option[T]] {
        case Some(value) => ev.write(value)
        case None => JsNull
      }
  }

  // 3
  def toJson[T: JsonWriter](v: T): JsValue = {
    JsonWriter[T].write(v)
  }

  // 4
  implicit class JsonSyntax[T](v: T){
    def toJson(implicit ev: JsonWriter[T]): JsValue = ev.write(v)
  }


  toJson("vfvffbf")
  toJson(12)
  toJson(Option(12))
  toJson(Option("cdvdvdvff"))
  "vdfvfvfvf".toJson
  12.toJson
  Option(12).toJson



  // 1 компонент
  trait Ordering[T]{
    def less(a: T, b: T): Boolean
  }

  object Ordering{
    def from[A](f: (A, A) => Boolean): Ordering[A] = (a: A, b: A) => f(a, b)

    implicit val intOrdering = Ordering.from[Int]((a, b) => a < b)

    implicit val strOrdering = Ordering.from[String]((a, b) => a < b)
  }

  // 3 имплисит параметр
  def greatest[A](a: A, b: A)(implicit ordering: Ordering[A]): A =
    if(ordering.less(a, b)) b else a

  greatest(5, 10)
  greatest("fdvd", "hello world")

  // 1 компонент
  trait Eq[T]{
    def ===(a: T, b: T): Boolean
  }

  object Eq{
    def apply[T](): Eq[T] = ???
    // 2 компонент
    implicit val eqStr: Eq[String] = (a: String, b: String) => a == b
  }

  // 4 компонент
  implicit class EqSyntax[T](a: T){
    // 3 компонент
    def ===(b: T)(implicit eq: Eq[T]): Boolean =
      eq.===(a, b)
  }

  val result = List("a", "b", "c").filter(str => str === "1")



}
