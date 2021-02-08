package basics

import basics.ControlStructures.Command.{Average, Divide, Max, Min, Sum}

import scala.io.Source

object ControlStructures {
  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)

  val EMPTY_LIST_MESSAGE: String = "Empty list"
  val ZERO_DIVISION_MESSAGE: String = "Zero division exception"
  val WHITESPACE: String = " "

  sealed trait Result
  final case class CalcResult(value: Double, input: Command) extends Result

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    x.toLowerCase.trim
      .replaceAll(" +", WHITESPACE)
      .split(WHITESPACE)
      .toList match {
      case "divide" :: x :: xs :: Nil => Right(Divide(x.toDouble, xs.toDouble))
      case "sum" :: x                 => Right(Sum(x.map(x => x.toDouble)))
      case "average" :: x             => Right(Average(x.map(x => x.toDouble)))
      case "min" :: x                 => Right(Min(x.map(x => x.toDouble)))
      case "max" :: x                 => Right(Max(x.map(x => x.toDouble)))
      case _                          => Left(ErrorMessage("Unknown command"))
    }
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x match {
      case Divide(x, y) =>
        if (y == 0) Left(ErrorMessage(ZERO_DIVISION_MESSAGE))
        else Right(CalcResult(x / y, Divide(x, y)))
      case Sum(list) =>
        if (list.isEmpty) Left(ErrorMessage(EMPTY_LIST_MESSAGE))
        else Right(CalcResult(list.sum, Sum(list)))
      case Average(list) =>
        if (list.isEmpty) Left(ErrorMessage(EMPTY_LIST_MESSAGE))
        else Right(CalcResult(list.sum / list.size, Average(list)))
      case Min(list) =>
        if (list.isEmpty) Left(ErrorMessage(EMPTY_LIST_MESSAGE))
        else Right(CalcResult(list.min, Min(list)))
      case Max(list) =>
        if (list.isEmpty) Left(ErrorMessage(EMPTY_LIST_MESSAGE))
        else Right(CalcResult(list.max, Max(list)))
    }
  }

  def renderResult(x: Result): String = {
    x match {
      case CalcResult(res, Divide(x, y)) => s"$x divided by $y is $res"
      case CalcResult(res, Sum(list)) =>
        s"the sum of ${list.mkString(" ")} is $res"
      case CalcResult(res, Average(list)) =>
        s"the average of ${list.mkString(" ")} is $res"
      case CalcResult(res, Min(list)) =>
        s"the minimum of ${list.mkString(" ")} is $res"
      case CalcResult(res, Max(list)) =>
        s"the maximum of ${list.mkString(" ")} is $res"
    }
  }

  def process(x: String): String = {
    val res = for {
      parsed <- parseCommand(x)
      calculated <- calculate(parsed)
    } yield calculated

    res match {
      case Right(x) => renderResult(x);
      case Left(e)  => e.value;
    }
  }

  def main(args: Array[String]): Unit =
    Source.stdin.getLines() map process foreach println
}
