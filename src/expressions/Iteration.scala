package expressions

import values._
import ui._

case class Iteration(val condition: Expression, val body: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    try {
      var result = null.asInstanceOf[Value]
      while (condition.execute(env).asInstanceOf[values.Boole] == Boole(true)) {
        result = body.execute(env)
      }
      result
    } catch {
      case e: TypeException => throw new TypeException
    }
  }
}