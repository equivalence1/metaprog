package ru.mit.supercompilation

object Types {
  sealed trait Expr
  case class Var(v: Int) extends Expr
  case class Lambda(e: Expr) extends Expr
  case class App(e1: Expr, e2: Expr) extends Expr
//  case class Let(e1: Expr, e2: Expr) extends Expr
//  case class Fun(s: String) extends Expr
//  case class Constr(s: String, es: List[Expr]) extends Expr
//  case class Case(e: Expr, cases: List[(String, List[Expr], Expr)]) extends Expr

  class Prog(e: Expr, funs: List[(String, Expr)])

  sealed trait ContextLevel
  case class AppCtx(e: Expr) extends ContextLevel
  case object LambdaCtx extends ContextLevel

  type Context = List[ContextLevel]
  type ReducedExpr = (Expr, Context)
}
