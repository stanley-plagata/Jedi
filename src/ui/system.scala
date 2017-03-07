package ui

import values._
import expressions._

object system {
  def execute(opcode: Identifier, args: List[Value]): Value = {
    opcode.name match {
      case "add"     => add(args)
      case "mul"     => mul(args)
      case "sub"     => sub(args)
      case "div"     => div(args)
      case "equals"  => equals(args)
      case "less"    => less(args)
      case "more"    => more(args)
      case "unequal" => unequal(args)
      case "content" => content(args)
      case "var"     => makeVar(args)
      case _         => throw new UndefinedException(opcode.name)
    }
  }

  private def add(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("addition expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all addition inputs must be numbers")
    val result = vals.map(_.asInstanceOf[Number])
    result.reduce(_ + _)
  }

  private def mul(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("multiplication expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all multiplication inputs must be numbers")
    val result = vals.map(_.asInstanceOf[Number])
    result.reduce(_ * _)
  }

  private def sub(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("subtraction expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all subtraction inputs must be numbers")
    val result = vals.map(_.asInstanceOf[Number])
    result.reduce(_ - _)
  }

  private def div(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("division expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all division inputs must be numbers")
    val result = vals.map(_.asInstanceOf[Number])
    result.reduce(_ / _)
  }

  private def equals(vals: List[Value]): Value = {
    if (vals.length != 2) throw new TypeException("equals expects 2 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("both equals inputs must be numbers")
    val result = (vals(0).asInstanceOf[Number].==(vals(1).asInstanceOf[Number]))
    result
  }

  private def less(vals: List[Value]): Value = {
    if (vals.length != 2) throw new TypeException("less expects 2 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("both less inputs must be numbers")
    val result = (vals(0).asInstanceOf[Number].<(vals(1).asInstanceOf[Number]))
    result
  }

  private def more(vals: List[Value]): Value = {
    if (vals.length != 2) throw new TypeException("more expects 2 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("both more inputs must be numbers")
    val result = (vals(0).asInstanceOf[Number].>(vals(1).asInstanceOf[Number]))
    result
  }

  private def unequal(vals: List[Value]): Value = {
    if (vals.length != 2) throw new TypeException("unequal expects 2 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("both unequal inputs must be numbers")
    val result = (vals(0).asInstanceOf[Number].!=(vals(1).asInstanceOf[Number]))
    result
  }

  private def content(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("content expects > 0 inputs")
    val result = vals.head.asInstanceOf[Variable].content
    result
  }

  private def makeVar(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("content expects > 0 inputs")
    val result = new Variable(vals.head)
    result
  }
}