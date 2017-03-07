package values

case class Boole(val value: Boolean) extends Value with expressions.Literal {
  def &&(other: Boole): Boole = Boole(this.value && other.value)
  def ||(other: Boole): Boole = Boole(this.value || other.value)
  def !(): Boole = Boole(!this.value)

  override def toString() = "" + value
}