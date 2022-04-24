package icu.harx

import scala.collection.mutable
import scala.io.StdIn

sealed trait Action
case class Shift(state_no: Int) extends Action
case class Reduce(rule_no: Int) extends Action
case object Accept              extends Action

class Parser(
    val rule_table: Map[Int, (Char, String)],
    val action_table: Map[(Int, Char), Action],
    val goto_table: Map[(Int, Char), Int]
) {
  /* 状态栈 初始为 0 */
  val state_stack: mutable.Stack[Int] = mutable.Stack(0)
  /* 符号栈 初始为 # */
  val symbol_stack: mutable.Stack[Char] = mutable.Stack('#')

  /* 分析函数 */
  def parse(str: String): Unit = {
    val input_stack = mutable.Stack(str.toCharArray*)

    while (true) {
      trace(input_stack)

      /* 当前状态 取状态栈栈顶 */
      val state_no = state_stack.top
      /* 当前输入 取输入栈栈顶 */
      val input_char = input_stack.top

      /* 当前动作 查动作表 */
      action_table((state_no, input_char)) match {
        case Shift(sno) =>
          /*
           * 移进(状态号):
           *   1. 状态栈 <-- (状态号)
           *   2. 符号栈 <-- (输入字符) <-- 输入栈
           */
          state_stack.push(sno)
          symbol_stack.push(input_stack.pop())
        case Reduce(rno) =>
          /*
           * 规约(规则号):
           *   1. 查规则表 取出规则的符号和推出串
           *   2. 依据推出串的长度出栈
           *   3. 将符号入栈
           *   4. 查转移表 将下一个状态入栈
           */
          val (sym, sym_string) = rule_table(rno)

          for (_ <- 1 to sym_string.length) {
            symbol_stack.pop()
            state_stack.pop()
          }

          symbol_stack.push(sym)
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
  def trace(input_stack: mutable.Stack[Char]): Unit = {
    val state  = state_stack.mkString("[", "", "]")
    val symbol = symbol_stack.mkString("[", "", "]")
    println(f"|state $state%30s|symbol $symbol%30s|remaining ${input_stack.mkString("[", "", "]")}%20s|")
  }
}

/* 执行入口 main函数 */
object Main {
  def main(args: Array[String]): Unit = {
    /* 规则表 */
    val rule_table =
      Map(
        (1, 'L' -> "E,L"),
        (2, 'L' -> "E"),
        (3, 'E' -> "a"),
        (4, 'E' -> "b")
      )

    /* 动作表 */
    val action_table =
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
    val goto_table =
      Map(
        (0, 'L') -> 1,
        (0, 'E') -> 2,
        (5, 'L') -> 6,
        (5, 'E') -> 2
      )

    while (true) {
      /* 接受输入 */
      println("Enter a string: ")
      var input = StdIn.readLine()
      if (!input.endsWith("#")) {
        input = input + "#"
      }

      /* 分析 */
      val parser = new Parser(rule_table, action_table, goto_table)
      parser.parse(input)
    }
  }
}
