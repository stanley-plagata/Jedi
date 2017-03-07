package expressions

import values._

case class Disjunction(val operands: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
    for (operand <- operands) {
      if (operand.execute(env) == Boole(true)) {
        return Boole(true)
      }
    }
    Boole(false)
  }
}