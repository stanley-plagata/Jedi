package values

case class Number(val value: Double) extends Value with expressions.Literal {
  def +(other: Number): Number = Number(this.value + other.value)
  def -(other: Number): Number = Number(this.value - other.value)
  def *(other: Number): Number = Number(this.value * other.value)
  def /(other: Number): Number = Number(this.value / other.value)
  def ==(other: Number): Boole = Boole(this.value == other.value)
  def <(other: Number): Boole = Boole(this.value < other.value)
  def >(other: Number): Boole = Boole(this.value > other.value)
  def !=(other: Number): Boole = Boole(this.value != other.value)

  override def toString() = "" + value
}