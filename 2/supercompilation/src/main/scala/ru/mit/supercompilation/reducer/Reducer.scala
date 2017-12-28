package ru.mit.supercompilation.reducer

object Reducer {

//  def reduce(expr: Expr, ctx: Context): ReducedExpr = {
//    expr match {
//      case Var(v) => (Var(v), ctx)
//      case Lambda(e) =>
//        ctx match {
//          case (AppCtx(e1)) :: xs => reduce(shift(-1, subst(e, shift(1, e1))), xs)
//          case _ =>
//            val (e1, iCtx) = reduce(e, Nil)
//            if (iCtx.nonEmpty)
//              (e1, iCtx ++ (LambdaCtx :: ctx))
//            else
//              (Lambda(e1), ctx) // TODO
//        }
//      case App(e1, e2) => reduce(e1, AppCtx(e2) :: ctx)
//    }
//  }

}
