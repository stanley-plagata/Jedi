package values

class Variable(var content: Value) extends Value {
  override def toString() = "Variable(" + content + ")"
}