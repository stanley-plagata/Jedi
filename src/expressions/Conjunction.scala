package expressions

import values._

case class Conjunction(val operands: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
    for (operand <- operands) {
      if (operand.execute(env) == Boole(false)) {
        return Boole(false)
      }
    }
    Boole(true)
  }
}