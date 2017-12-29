package ru.mit.supercompilation

/**
  * Common types for all stages of supercompilation
  */
object Types {

  // Expressions {{{

  sealed trait Expr
  sealed trait Var
  case class BVar(v: Int) extends Expr with Var // bounded variable
  // unbounded variable. We only generate them during generalization and make them
  // function's arguments in the end
  case class ConfVar(id: Int) extends Expr with Var with Ordered[ConfVar] {
    // TODO only need it in generalization -- move it there
    import scala.math.Ordered.orderingToOrdered
    def compare(that: ConfVar): Int = (this.id, this.id) compare (that.id, that.id)
  }
  case class GlobalVar(name: String) extends Expr with Var // We will never substitute into global var, so just use string, not index
  case class Lambda(e: Expr) extends Expr
  case class App(e1: Expr, e2: Expr) extends Expr
  case class Let(e1: Expr, e2: Expr) extends Expr
  case class Fun(name: String) extends Expr
  case class Constr(name: String, es: List[Expr]) extends Expr
  case class Case(selector: Expr, cases: List[(String, Int, Expr)]) extends Expr

  // }}}

  type FunDef = (String, Expr)
  type Program = (Expr, List[FunDef])

  type Substitution = List[(ConfVar, Expr)]
}
