package expressions

import values._
import ui._

case class Lambda(val parameters: List[Identifier], val body: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    new Closure(parameters, body, env)
  }
}