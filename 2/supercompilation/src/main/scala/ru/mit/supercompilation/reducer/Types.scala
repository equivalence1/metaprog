package ru.mit.supercompilation.reducer

import ru.mit.supercompilation.Types.Expr

object Types {
  sealed trait ContextLevel
  //case class LetCtx(e: Expr) extends ContextLevel
  case class AppCtx(e: Expr) extends ContextLevel
  case class CaseCtx(cases: List[(String, Int, Expr)]) extends ContextLevel

  type Context = List[ContextLevel]
  type Observable = Expr
  type RedInCon = (Expr, Context)
  type NormalizedExpr = Either[Observable, RedInCon]
  type NormalizedProg = (NormalizedExpr, List[(String, Expr)])
}
