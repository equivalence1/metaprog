package ru.mit.supercompilation

/**
  * Common types for all stages of supercompilation
  */
object Types {

  // Expr-related

  sealed trait Expr
  case class Var(v: Int) extends Expr
  case class Lambda(e: Expr) extends Expr
  case class App(e1: Expr, e2: Expr) extends Expr
  //  case class Let(e1: Expr, e2: Expr) extends Expr
  //  case class Fun(s: String) extends Expr
  //  case class Constr(s: String, es: List[Expr]) extends Expr
  //  case class Case(e: Expr, cases: List[(String, List[Expr], Expr)]) extends Expr

  type Program = (Expr, List[(String, Expr)])
}
