package values

class Notification(val message: String) extends Value {
  override def toString() = message
}

object Notification {
  val VARUPDATE = Notification("variable updated")
  val BINDCREATE = Notification("binding created")
  val UNKNOWN = Notification("unknown")
  val ERROR = Notification("error")
  val OK = Notification("ok")
  val DONE = Notification("done")
  
  def apply(msg: String) = new Notification(msg)
}