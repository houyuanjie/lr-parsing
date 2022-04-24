package icu.harx

import icu.harx.Parser.{Accept, Action, Reduce, Rule, RuleNo, Shift, StateNo, Symbol}

import scala.collection.mutable
import scala.io.StdIn

object Parser {
  /* 类型定义 */
  // 状态号
  type StateNo = Int
  // 规则号
  type RuleNo = Int
  // 符号
  type Symbol = Char
  // 规则 (符号, [字母|符号]) 的二元组
  type Rule = (Symbol, List[Either[Char, Symbol]])

  // 动作
  sealed trait Action
  // 移进
  case class Shift(state_no: StateNo) extends Action
  // 规约
  case class Reduce(rule_no: RuleNo) extends Action
  // 接受
  case object Accept extends Action
}

class Parser(
    /* 输入字符列表 */
    val input: List[Char],
    /* 符号表 */
    val symbols: List[Symbol],
    /* 规则表 */
    val rule_table: Map[RuleNo, Rule],
    /* 动作表 */
    val action_table: Map[(StateNo, Char), Action],
    /* 转移表 */
    val goto_table: Map[(StateNo, Symbol), StateNo]
) {
  require(input.last.equals('#'), "Input must end with '#'")

  /* 输入栈 初始为所有输入字符 */
  val input_stack: mutable.Stack[Char] = mutable.Stack[Char](input*)
  /* 状态栈 初始为 0 */
  val state_stack: mutable.Stack[Int] = mutable.Stack(0)
  /* 符号栈 初始为 # */
  val symbol_stack: mutable.Stack[Either[Char, Symbol]] = mutable.Stack(Left('#'))

  /* 分析函数 */
  def parse(): Unit = {
    while (true) {
      /* 打印堆栈 */
      trace()

      /* 当前状态 取状态栈栈顶 */
      val state_no: StateNo = state_stack.top
      /* 当前输入 取输入栈栈顶 */
      val input_char: Char = input_stack.top

      /* 当前动作 查动作表 */
      val action: Action = action_table((state_no, input_char))
      action match {
        case Shift(sno) =>
          /*
           * 移进(状态号):
           *   1. 状态栈 <-- (状态号)
           *   2. 符号栈 <-- (输入字符) <-- 输入栈
           */
          state_stack.push(sno)
          symbol_stack.push(Left(input_stack.pop()))
        case Reduce(rno) =>
          /*
           * 规约(规则号):
           *   1. 查规则表 取出规则的符号和推出串
           *   2. 依据推出串的长度出栈
           *   3. 将符号入栈
           *   4. 查转移表 将下一个状态入栈
           */
          val (sym, sym_string) = rule_table(rno)
          val length            = sym_string.length

          for (_ <- 1 to length) {
            symbol_stack.pop()
            state_stack.pop()
          }

          symbol_stack.push(Right(sym))
          state_stack.push(goto_table((state_stack.top, sym)))
        case Accept =>
          /*
           * 接受:
           *   1. 接受
           *   2. 退出函数
           */
          println("accept")
          return
      }
    }
  }

  /* 跟踪函数 将堆栈信息打印到控制台 */
  def trace(): Unit = {
    val state = state_stack.mkString("[", "", "]")
    val symbol = symbol_stack
      .map {
        case Left(char)    => char
        case Right(symbol) => symbol
      }
      .mkString("[", "", "]")
    val remaining_input = input_stack.mkString("[", "", "]")

    println(f"|state $state%30s|symbol $symbol%30s|remaining $remaining_input%20s|")
  }
}

/* 执行入口 main函数 */
object Main {
  def main(args: Array[String]): Unit = {
    /* 接受输入 */
    println("Enter a string: ")
    val in = StdIn.readLine()
    val input: List[Char] =
      if (in.endsWith("#")) { in.toList }
      else { in.toList :+ '#' }

    /* 符号 */
    val symbols: List[Symbol] = List('L', 'E')
    /* 规则表 */
    val rule_table: Map[RuleNo, Rule] =
      Map(
        (1, 'L' -> (Right('E') :: Left(',') :: Right('L') :: Nil)),
        (2, 'L' -> (Right('E') :: Nil)),
        (3, 'E' -> (Left('a') :: Nil)),
        (4, 'E' -> (Left('b') :: Nil))
      )

    /* 动作表 */
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

    /* 转移表 */
    val goto_table: Map[(StateNo, Symbol), StateNo] =
      Map(
        (0, 'L') -> 1,
        (0, 'E') -> 2,
        (5, 'L') -> 6,
        (5, 'E') -> 2
      )

    /* 实例化分析对象 */
    val parser = new Parser(input, symbols, rule_table, action_table, goto_table)
    parser.parse()
  }
}
