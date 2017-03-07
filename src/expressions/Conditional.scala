package expressions

import values._

case class Conditional(val operand1: Expression, val operand2: Expression, val operand3: Expression = null) extends SpecialForm {
  def execute(env: Environment): Value = {
    if (operand1.execute(env) == Boole(true)) {
      operand2.execute(env)
    } else {
      operand3.execute(env)
    }
  }
}