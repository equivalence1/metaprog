package ru.mit.supercompilation

/**
  * Common types for all stages of supercompilation
  */
object Types {

  // Expr-related

  sealed trait Expr
  case class Var(v: Int) extends Expr // bounded variable
  case class GlobalVar(name: String) extends Expr // We will never substitute into global var, so just use string, not index
  case class Lambda(e: Expr) extends Expr
  case class App(e1: Expr, e2: Expr) extends Expr
  case class Let(e1: Expr, e2: Expr) extends Expr
  case class Fun(s: String) extends Expr
  case class Constr(name: String, es: List[Expr]) extends Expr
  case class Case(e: Expr, cases: List[(String, Int, Expr)]) extends Expr

  type Program = (Expr, List[(String, Expr)])
}
