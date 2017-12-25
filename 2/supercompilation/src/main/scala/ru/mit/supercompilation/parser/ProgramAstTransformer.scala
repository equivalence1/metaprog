package ru.mit.supercompilation.parser

import ru.mit.supercompilation.Types._

/**
  * Converts ExprAst produced by Parser into Expr
  * The main complication is that ExprAst uses string variables
  * and Types.Expr uses de Bruijn indexes
  */
object ProgramAstTransformer {

//  val functionsSet: Set[String]
//  val globalVariablesIds: Map[String, Int]

  private def buildExpr(ast: ExprAst, scopeVariablesStack: List[String]): Expr = {
    ast match {
      case IdentifierNode(name) => Var(scopeVariablesStack.indexOf(name))
      case LambdaNode(vars, e) => vars match {
        case x :: xs => Lambda(buildExpr(LambdaNode(xs, e), x :: scopeVariablesStack))
        case Nil => buildExpr(e, scopeVariablesStack)
      }
      case AppNode(e1, e2) => App(buildExpr(e1, scopeVariablesStack), buildExpr(e2, scopeVariablesStack))
    }
  }

  def apply(prog: ParsedProgram): Program = {
 //   build(ast, Nil)
    null
  }
}
