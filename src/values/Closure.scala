package values

import expressions._

class Closure(val params: List[Identifier], val body: Expression, val defEnv: Environment) extends Value {
  def apply(args: List[Value]): Value = {
    val tempEnv = new Environment(defEnv)
    tempEnv.put(params, args)
    body.execute(tempEnv)
  }
}