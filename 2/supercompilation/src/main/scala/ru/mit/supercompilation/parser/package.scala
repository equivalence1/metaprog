package ru.mit.supercompilation

/**
  * package object for common types we use during parsing stage.
  */
package object parser {
  // Lexer related

  sealed trait Token
  case class  IDENTIFIER(s: String) extends Token
  case class  CONSTRUCTOR(s: String) extends Token
  case object LAMBDA extends Token
  case object DOT extends Token
  case object OPEN_BRACKET extends Token
  case object CLOSE_BRACKET extends Token
  case object IN extends Token
  case object CURLY_OPEN extends Token
  case object CURLY_CLOSE extends Token
  case object CASE extends Token
  case object OF extends Token
  case object ALTERNATION extends Token
  case object ARROW extends Token
  case object ASSIGNMENT extends Token
  case object WHERE extends Token
  case object SEMICOLON extends Token

  // Parser related

  sealed trait ExprAst
  // It's impossible to distinguish variables from functions at this point,
  // only ExprBuilder can do this.
  // Thus IdentifierNode, not VarNode and FunNode
  case class IdentifierNode(name: String) extends ExprAst
  case class ConstructorNode(name: String, args: List[ExprAst]) extends ExprAst
  case class LambdaNode(varNames: List[String], e: ExprAst) extends ExprAst
  case class AppNode(e1: ExprAst, e2: ExprAst) extends ExprAst
  case class CaseConstructorNode(name: String, args: List[String]) extends ExprAst
  case class CaseNode(selector: ExprAst, cases: List[(CaseConstructorNode, ExprAst)]) extends ExprAst

  type ProgramAst = (ExprAst, List[(String, ExprAst)])
}
