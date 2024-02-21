package scala3_2

import scala.annotation.targetName

object homework1 {
  extension (x: String)
    def +:(y: String) : Int = {
      (x + y).toInt
    }

  @main def part1Ex(): Unit = {
    val result = "1" +: "33"
    println(result)
    println(result.getClass)
  }
}

object homework2 {

  enum CompletionArg {
    case IntArg(value: Int)
    case StringArg(value: String)
    case FloatArg(value: Float)
  }

  object CompletionArg {
    given fromString: Conversion[String, CompletionArg] = StringArg(_)

    given fromInt: Conversion[Int, CompletionArg] = IntArg(_)

    given fromFloat: Conversion[Float, CompletionArg] = FloatArg(_)
  }

  private def complete(arg: CompletionArg): String = arg match
    case CompletionArg.IntArg(value) => s"int value $value"
    case CompletionArg.StringArg(value) => s"string value $value"
    case CompletionArg.FloatArg(value) => s"float value $value"


  import CompletionArg.*

  @main def part2Ex(): Unit ={
    println(complete("String"))
    println(complete(1))
    println(complete(7f))
  }
}

object homework3 {
  opaque type Logarithm = Double

  object Logarithm {
    def apply(d: Double): Logarithm = math.log(d)

    def safe(d: Double): Option[Logarithm] =
      if d > 0 then Some(math.log(d)) else None
  }

  extension (x: Logarithm)

    def toDouble: Double = math.exp(x)

    def +(y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))

    def *(y: Logarithm): Logarithm = x + y


  @main def part3Ex(): Unit = {
    val l = Logarithm(1.0)
    val l2 = Logarithm(2.0)
    val l3 = l * l2
    val l4 = l + l2
  }
}