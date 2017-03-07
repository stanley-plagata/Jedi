package ui

import scala.util.parsing.combinator._
import expressions._
import values._

class SithParsers extends RegexParsers {

  def expression: Parser[Expression] = declaration | conditional | iteration | assignment | disjunction | failure("Invalid expression")

  def declaration: Parser[Declaration] = "def" ~ identifier ~ "=" ~ expression ^^
    {
      case "def" ~ id ~ "=" ~ exp => Declaration(id, exp)
    }

  def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^
    {
      case "if" ~ "(" ~ exp1 ~ ")" ~ exp2 ~ None                => Conditional(exp1, exp2)
      case "if" ~ "(" ~ exp1 ~ ")" ~ exp2 ~ Some("else" ~ exp3) => Conditional(exp1, exp2, exp3)
    }

  // DISJUNCTION ::= CONJUNCTION ~ (|| ~ CONJUNCTION)*
  def disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^
    {
      case con ~ Nil  => con
      case con ~ cons => Disjunction(con :: cons)
    }

  // CONJUNCTION ::= EQUALITY ~ (&& ~ EQUALITY)*
  def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^
    {
      case eq ~ Nil => eq
      case eq ~ eqs => Conjunction(eq :: eqs)
    }

  // EQUALITY ::= INEQUALITY ~ (== ~ INEQUALITY)*
  def equality: Parser[Expression] = inequality ~ rep("==" ~> inequality) ^^
    {
      case ineq ~ Nil   => ineq
      case ineq ~ ineqs => FunCall(Identifier("equals"), ineq :: ineqs)
    }

  // INEQUALITY ::= SUM ~ ((< | > | !=) ~ SUM)?
  def inequality: Parser[Expression] = sum ~ opt(("<" | ">" | "!=") ~ sum) ^^
    {
      case sum ~ None               => sum
      case sum1 ~ Some("<" ~ sum2)  => FunCall(Identifier("less"), List(sum1, sum2))
      case sum1 ~ Some(">" ~ sum2)  => FunCall(Identifier("more"), List(sum1, sum2))
      case sum1 ~ Some("!=" ~ sum2) => FunCall(Identifier("unequals"), List(sum1, sum2))
    }

  // exp -> 0 - exp
  def negate(exp: Expression): Expression = {
    val sub = Identifier("sub")
    val zero = Number(0)
    FunCall(sub, List(zero, exp))
  }

  // SUM ::= PRODUCT ~ ((\+|-) ~ PRODUCT)*
  def sum: Parser[Expression] = product ~ rep(("+" | "-") ~ product ^^ { case "+" ~ s => s case "-" ~ s => negate(s) }) ^^
    {
      case prod ~ Nil   => prod
      case prod ~ prods => FunCall(Identifier("add"), prod :: prods)
    }

  // exp => 1/exp
  def invert(exp: Expression): Expression = {
    val div = Identifier("div")
    val one = Number(1)
    FunCall(div, List(one, exp))
  }

  // PRODUCT ::= TERM ~ ((\* | /)~ TERM)*
  def product: Parser[Expression] = term ~ rep(("*" | "/") ~ term ^^ { case "*" ~ s => s case "/" ~ s => invert(s) }) ^^
    {
      case term ~ Nil   => term
      case term ~ terms => FunCall(Identifier("mul"), term :: terms)
    }

  // TERM ::= LAMBDA | BLOCK | LITERAL | FUNCALL | IDENTIFIER | (~EXPRESSION~)
  def term: Parser[Expression] = lambda | block | literal | funcall | identifier | "(" ~> expression <~ ")"

  // OPERANDS ::= (~(EXPRESSION ~ (,~EXPRESSION)*)?~)
  // i.e. OPERANDS ::= A comma-separated list of 0 or more expressions bracketed by parentheses
  def operands: Parser[List[Expression]] = "(" ~> opt(expression ~ rep("," ~> expression)) <~ ")" ^^
    {
      case None             => Nil
      case Some(exp ~ Nil)  => List(exp)
      case Some(exp ~ exps) => exp :: exps
      case _                => Nil
    }

  // FUNCALL ::= IDENTIFIER ~ OPERANDS
  def funcall: Parser[Expression] = (deref | "(" ~> lambda <~ ")" | identifier) ~ opt(operands) ^^
    {
      case id ~ None      => id
      case id ~ Some(Nil) => FunCall(id, Nil)
      case id ~ Some(ops) => FunCall(id, ops)
    }

  // LITERAL ::= BOOLE | NUMERAL
  def literal: Parser[Literal] = boole | numeral

  // NUMERAL ::= (\+|-)?~DIGIT~(.~DIGIT+)?
  def numeral: Parser[Number] = """(\+|-)?[0-9]+(\.[0-9]+)?""".r ^^
    {
      case num => Number(num.toDouble)
    }

  // BOOLE ::= true | false
  def boole: Parser[Boole] = ("true" | "false") ^^
    {
      case "true" => Boole(true) case "false" => Boole(false)
    }

  // IDENTIFIER ::= LETTER~(LETTER | DIGIT)*
  def identifier: Parser[Identifier] = """[a-zA-z][0-9a-zA-Z]*""".r ^^
    {
      case id => Identifier(id)
    }

  // PARAMETERS ::= ((IDENTIFIER (, IDENTIFIER)*))?)
  def parameters: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^
    {
      case None           => Nil
      case Some(id ~ Nil) => List(id)
      case Some(id ~ ids) => id :: ids
      case _              => Nil
    }

  // BLOCK ::= {EXPRESSION (; EXPRESSION)*}
  def block: Parser[Expression] = "{" ~> expression ~ rep(";" ~> expression) <~ "}" ^^
    {
      case exp ~ Nil  => exp
      case exp ~ exps => Block(exp :: exps)
    }

  // LAMBDA ::= lambda PARAMETERS EXPRESSION
  def lambda: Parser[Expression] = "lambda" ~ parameters ~ expression ^^
    {
      case "lambda" ~ params ~ exp => Lambda(params, exp)
    }

  // ITERATION ::= while (EXPRESSION) EXPRESSION
  def iteration: Parser[Iteration] = "while" ~ "(" ~ expression ~ ")" ~ expression ^^
    {
      case "while" ~ "(" ~ exp1 ~ ")" ~ exp2 => Iteration(exp1, exp2)
    }

  // ASSIGNMENT ::= IDENTIFIER = EXPRESSION
  def assignment: Parser[Assignment] = identifier ~ "=" ~ expression ^^
    {
      case id ~ "=" ~ exp => Assignment(id, exp)
    }

  // DEREF ::= [EXPRESSION]
  def deref: Parser[Expression] = "[" ~> expression <~ "]" ^^
    {
      case exp => FunCall(Identifier("content"), List(exp))
    }
}