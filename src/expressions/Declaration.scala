package expressions

import values._

case class Declaration(val id: Identifier, val exp: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    env.put(id, exp.execute(env))
    Notification.OK
  }
}