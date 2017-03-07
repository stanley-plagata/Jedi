package expressions

import values._

trait Literal extends Value with Expression with Serializable {
  def execute(env: Environment): Value = this
}