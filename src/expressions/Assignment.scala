package expressions

import values._
import ui._

case class Assignment(val vbl: Identifier, val update: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    if (vbl.execute(env).isInstanceOf[Variable]) {
      vbl.execute(env).asInstanceOf[Variable].content = update.execute(env)
    } else throw new UndefinedException
    Notification.DONE
  }
}