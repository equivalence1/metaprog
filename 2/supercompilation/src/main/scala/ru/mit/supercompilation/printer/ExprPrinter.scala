package ru.mit.supercompilation.printer

import ru.mit.supercompilation.Types._
import ru.mit.supercompilation.reducer.Types._

object ExprPrinter {

  var sb: StringBuilder = _

  private def print(e: Expr, namesStack: List[String]): Unit = {
    e match {
      case Var(n) =>
        if (n >= namesStack.size) sb.append("out" + (n - namesStack.size))
        else sb.append(namesStack(n))
      case Lambda(le) =>
        val varName = "x" + namesStack.size
        sb.append("(\\" + varName + " . ")
        print(le, varName :: namesStack)
        sb.append(")")
      case App(e1, e2) =>
        sb.append("(")
        print(e1, namesStack)
        sb.append(" ")
        print(e2, namesStack)
        sb.append(")")
    }
  }

  private def restoreExpr(rExpr: ReducedExpr): Expr = {
    rExpr match {
      case (e, Nil) => e
      case (e, LambdaCtx :: ctx) => restoreExpr(Lambda(e), ctx)
      case (e, (AppCtx(e1)) :: ctx) => restoreExpr(App(e, e1), ctx)
    }
  }

  def apply(e: Expr): String = {
    sb = new StringBuilder
    print(e, Nil)
    sb.toString()
  }

  def apply(rExpr: ReducedExpr): String = {
    ExprPrinter(restoreExpr(rExpr))
  }

}
