package expressions

import values._

case class Block(val locals: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
    val tempEnv = new Environment(env)
    var result = null.asInstanceOf[Value]
    for (local <- locals) result = local.execute(tempEnv)
    result
  }
}