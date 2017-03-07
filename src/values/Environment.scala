package values

import scala.collection.mutable.HashMap
import expressions.Identifier

class Environment(var nextEnv: Environment = null) extends HashMap[Identifier, Value] with Value {
  def put(names: List[Identifier], vals: List[Value]) {
    try {
      for (i <- 0 until names.length) this.put(names(i), vals(i))
    } catch {
      case e: IndexOutOfBoundsException => println("Error: 'names' and 'vals' must be the same length")
    }
  }

  def find(id: Identifier): Value = {
    if (this.contains(id) == true) {
      this(id)
    } else if (nextEnv != null) {
      nextEnv.find(id)
    } else {
      Notification.UNKNOWN
    }
  }
}