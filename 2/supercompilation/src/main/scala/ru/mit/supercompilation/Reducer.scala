package ru.mit.supercompilation

import ru.mit.supercompilation.Types._

object Reducer {

  private def shift(from: Int, k: Int, expr: Expr): Expr = {
    expr match {
      case Var(v) => if (v >= from) Var(v + k) else Var(v)
      case Lambda(e) => Lambda(shift(from + 1, k, e))
      case App(e1, e2) => App(shift(from, k, e1), shift(from, k, e2))
      case _ => throw new RuntimeException("this case is not yet implemented")
    }
  }

  private def subst(index: Int, origE: Expr, substE: Expr): Expr = {
    origE match {
      case Var(v) => if (v == index) substE else Var(v)
      case Lambda(e) => Lambda(subst(index + 1, e, shift(0, 1, substE)))
      case App(e1, e2) => App(subst(index, e1, substE), subst(index, e2, substE))
      case _ => throw new RuntimeException("this case is not yet implemented")
    }
  }

  def reduce(expr: Expr, ctx: Context): ReducedExpr = {
    expr match {
      case Var(v) => (Var(v), ctx)
      case Lambda(e) =>
        ctx match {
          case (AppCtx(e1)) :: xs => reduce(shift(0, -1, subst(0, e, shift(0, 1, e1))), xs)
          case _ =>
            val (e1, iCtx) = reduce(e, Nil)
            if (iCtx.nonEmpty)
              (e1, iCtx ++ (LambdaCtx :: ctx))
            else
              (Lambda(e1), ctx) // TODO
        }
      case App(e1, e2) => reduce(e1, AppCtx(e2) :: ctx)
    }
  }

}
