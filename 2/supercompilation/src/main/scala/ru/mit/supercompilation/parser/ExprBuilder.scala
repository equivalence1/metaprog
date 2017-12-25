package ru.mit.supercompilation.parser

import ru.mit.supercompilation.Types._

/**
  * Converts ExprAst produced by Parser into Types.Expr
  * The main complication is that ExprAst uses string variables
  * and Types.Expr uses de Bruijn indexes
  */
object ExprBuilder {
  private def build(ast: ExprAst, scopeVariablesStack: List[String]): Expr = {
    ast match {
      case IdentifierNode(name) => Var(scopeVariablesStack.indexOf(name))
      case LambdaNode(vars, e) => vars match {
        case x :: xs => Lambda(build(LambdaNode(xs, e), x :: scopeVariablesStack))
        case Nil => build(e, scopeVariablesStack)
      }
      case AppNode(e1, e2) => App(build(e1, scopeVariablesStack), build(e2, scopeVariablesStack))
    }
  }

  def apply(ast: ExprAst): Expr = {
    build(ast, Nil)
  }
}
