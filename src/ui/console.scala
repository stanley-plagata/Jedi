package ui

import values._
import expressions._

object console {
  val parsers = new SithParsers
  val globalEnv = new Environment

  def execute(cmmd: String): String = {
    val tree = parsers.parseAll(parsers.expression, cmmd)
    tree match {
      case t: parsers.Failure => throw new SyntaxException(t)
      case _ => {
        val exp = tree.get
        val result = exp.execute(globalEnv)
        result.toString()
      }
    }
  }

  def repl {
    var more = true
    while (more) {
      try {
        print("-> ")
        val cmmd = readLine()
        if (cmmd == "quit") {
          more = false
          println("Bye")
        } else {
          println(execute(cmmd))
        }
      } catch {
        case s: SyntaxException => {
          println(s.gripe)
          println(s.result.msg)
          println("line # = " + s.result.next.pos.line)
          println("column # = " + s.result.next.pos.column)
          println("token = " + s.result.next.first)
        }
        case t: TypeException => {
          println(t.gripe)
        }
        case u: UndefinedException => {
          println(u.gripe)
        }
      } finally {
        Console.flush
      }
    }
  }

  def main(args: Array[String]): Unit = { repl }

}