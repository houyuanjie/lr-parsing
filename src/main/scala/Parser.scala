import Parser.*

import scala.collection.mutable
import scala.io.StdIn

object Parser {
  type StateNo = Int
  type RuleNo  = Int
  type Symbol  = Char
  type Rule    = (Symbol, List[Either[Char, Symbol]])

  sealed trait Action
  case class Shift(state_no: StateNo) extends Action
  case class Reduce(rule_no: RuleNo)  extends Action
  case object Accept                  extends Action

  def main(args: Array[String]): Unit = {
    println("Enter a string: ")
    val in = StdIn.readLine()
    val input: List[Char] =
      if (in.endsWith("#")) { in.toList }
      else { in.toList :+ '#' }

    val symbols: List[Symbol] = List('L', 'E')
    val rule_table: Map[RuleNo, Rule] =
      Map(
        (1, 'L' -> (Right('E') :: Left(',') :: Right('L') :: Nil)),
        (2, 'L' -> (Right('E') :: Nil)),
        (3, 'E' -> (Left('a') :: Nil)),
        (4, 'E' -> (Left('b') :: Nil))
      )

    val action_table: Map[(StateNo, Char), Action] =
      Map(
        (0, 'a') -> Shift(3),
        (0, 'b') -> Shift(4),
        (1, '#') -> Accept,
        (2, ',') -> Shift(5),
        (2, '#') -> Reduce(2),
        (3, ',') -> Reduce(3),
        (3, '#') -> Reduce(3),
        (4, ',') -> Reduce(4),
        (4, '#') -> Reduce(4),
        (5, 'a') -> Shift(3),
        (5, 'b') -> Shift(4),
        (6, '#') -> Reduce(1)
      )

    val goto_table: Map[(StateNo, Symbol), StateNo] =
      Map(
        (0, 'L') -> 1,
        (0, 'E') -> 2,
        (5, 'L') -> 6,
        (5, 'E') -> 2
      )

    val parser = new Parser(input, symbols, rule_table, action_table, goto_table)
    parser.parse()
  }
}

class Parser(
    val input: List[Char],
    val symbols: List[Symbol],
    val rule_table: Map[RuleNo, Rule],
    val action_table: Map[(StateNo, Char), Action],
    val goto_table: Map[(StateNo, Symbol), StateNo]
) {
  require(input.last.equals('#'), "Input must end with '#'")

  val input_stack: mutable.Stack[Char]                  = mutable.Stack[Char](input*)
  val state_stack: mutable.Stack[Int]                   = mutable.Stack(0)
  val symbol_stack: mutable.Stack[Either[Char, Symbol]] = mutable.Stack(Left('#'))

  def parse(): Unit = {
    while (true) {
      trace()

      val state_no: StateNo = state_stack.top
      val input_char: Char  = input_stack.top

      val action: Action = action_table((state_no, input_char))
      action match {
        case Shift(sno) =>
          state_stack.push(sno)
          symbol_stack.push(Left(input_stack.pop()))
        case Reduce(rno) =>
          val (sym, sym_string) = rule_table(rno)
          val length            = sym_string.length

          for (_ <- 1 to length) {
            symbol_stack.pop()
            state_stack.pop()
          }

          symbol_stack.push(Right(sym))
          state_stack.push(goto_table((state_stack.top, sym)))
        case Accept =>
          println("accept")
          return
      }
    }
  }

  def trace(): Unit = {
    val state = state_stack.mkString("[", ",", "]")
    val symbol = symbol_stack
      .map {
        case Left(char)    => char
        case Right(symbol) => symbol
      }
      .mkString("[", "", "]")
    val remaining_input = input_stack.mkString

    println(f"|state $state%30s|symbol $symbol%30s|remaining $remaining_input%20s|")
  }
}
