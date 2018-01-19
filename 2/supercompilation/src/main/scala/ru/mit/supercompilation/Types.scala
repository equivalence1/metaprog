package ru.mit.supercompilation

import scala.language.implicitConversions

/**
  * Common types for all stages of supercompilation
  */
object Types {

  // Expressions {{{

  case class CaseBranch(constrName: String, nrArgs: Int, expr: Expr)

  sealed trait Expr
  sealed trait Var extends Expr
  case class BVar(v: Int) extends Var // bounded variable
  // unbounded variable. We only generate them during generalization and make them
  // function's arguments in the end
  case class ConfVar(id: Int) extends Var with Ordered[ConfVar] {
    // TODO only need it in generalization -- move it there
    import scala.math.Ordered.orderingToOrdered
    def compare(that: ConfVar): Int = (this.id, this.id) compare (that.id, that.id)
  }
  case class GlobalVar(name: String) extends Var // We will never substitute into global var, so just use string, not index
  case class Lambda(e: Expr) extends Expr
  case class App(e1: Expr, e2: Expr) extends Expr
  case class Let(subst: Substitution, e2: Expr) extends Expr
  case class Fun(name: String) extends Expr
  case class Constr(name: String, es: List[Expr]) extends Expr
  case class Case(selector: Expr, cases: List[CaseBranch]) extends Expr

  case class FDef(fName: String, body: Expr)
  case class Program(mainExpr: Expr, fdefs: List[FDef])

  // }}}

  // Normalized Expressions {{{

  sealed trait ContextLevel
  case class AppCtx(e: Expr) extends ContextLevel
  case class CaseCtx(cases: List[CaseBranch]) extends ContextLevel

  type Context = List[ContextLevel]
  type Observable = Expr
  type RedexInContext = (Expr, Context)
  type NormalizedExpr = Either[Observable, RedexInContext]
  case class NormalizedProg(NormalizedExpr: NormalizedExpr, fdefs: List[FDef])

  // }}}

  case class Generalization(gExpr: Expr, subst1: Substitution, subst2: Substitution)

  /**
    * So that I don't need to write "Generalization(...)" every time
    */
  implicit def tuple2Generalization(tuple3: (Expr, Substitution, Substitution)): Generalization =
    Generalization(tuple3._1, tuple3._2, tuple3._3)

  type Substitution = List[(ConfVar, Expr)]

}
