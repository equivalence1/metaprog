package ru.mit.supercompilation.reducer

import ru.mit.supercompilation.Types.Expr

object Types {
  sealed trait ContextLevel
  case class AppCtx(e: Expr) extends ContextLevel
  case object LambdaCtx extends ContextLevel

  type Context = List[ContextLevel]
  type ReducedExpr = (Expr, Context)

  type ReducedProg = (ReducedExpr, List[(String, Expr)])
}
