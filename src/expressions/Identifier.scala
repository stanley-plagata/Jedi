package expressions

import values._
import ui._

case class Identifier(val name: String) extends Expression with Serializable {
  def execute(env: Environment): Value = {
    if (env.find(this) != Notification.UNKNOWN) {
      env.find(this)
    } else throw new UndefinedException("Undefined identifier: " + this.name)
  }
}