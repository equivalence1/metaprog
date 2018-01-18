package ru.mit.supercompilation

import ru.mit.supercompilation.Types._

object Reducer {

  /**
    * reduction step:
    *
    * C {ei}                                 -> C {ei}
    * \v -> e                                -> \v -> e
    * con[f]                                 -> con[unfold(f)]
    * con[(\v -> e0) e1]                     -> con[e0{v := e1}]
    * con[case Cj {e'k} of {Ci {vik} -> ei}] -> con[ej{{vjk} := {e'k}}]
    */
  def nReduceStep(expr: NormalizedExpr, fdefs: List[FDef]): NormalizedExpr = {
    expr match {
      case Right((Fun(name), ctx)) =>
        normalize(unfold(name, fdefs), ctx)
      case Right((App(Lambda(e1), e2), ctx)) =>
        normalize(shift(-1, subst(e1, shift(1, e2))), ctx)
      case Right((Case(Constr(name, es), cases), ctx)) =>
        val _case = cases.find(_.constrName == name).get
        val newExpr: Expr = 0.until(_case.nrArgs).reverse.foldLeft(_case.expr) { (e, id) =>
          shift(-1, subst(e, shift(1, es(id))))
        }
        normalize(newExpr, ctx)
      case Right((Let(s, e), ctx)) =>
        normalize(subst(e, s), ctx)
      case _ => expr
    }
  }

  // This function is mostly for testing purposes right now.
  // But who knows, it might be useful later
  def nReduce(nExpr: NormalizedExpr, fdefs: List[FDef]): NormalizedExpr = {
    var oldResult: NormalizedExpr = null
    var newResult = nExpr
    while (!newResult.equals(oldResult)) {
      oldResult = newResult
      val res = nReduceStep(oldResult, fdefs)
      newResult = res
    }
    newResult
  }

  /**
    * obs := v {ei}
    *      | C {ei}
    *      | (\v -> e)
    *
    * con := []
    *      | con e                       -- App [] e
    *      | case con of {pi -> vi}      -- case [] of ...
    *
    * red := fun
    *      | (\v -> e) e1
    *      | case v {ei} of {pi -> vi'}
    *      | case C {ei} of {pi -> vi'}
    *
    * normalized form = obs | con[red]
    */
  def normalize(expr: Expr, ctx: Context): NormalizedExpr = {
    def varHelper(variable: Var, ctx: Context): NormalizedExpr = {
      val apps = ctx.takeWhile(_.isInstanceOf[AppCtx])
      val tmpCtx = ctx.drop(apps.size)
      val tmpExpr = apps.foldLeft(variable.asInstanceOf[Expr]) {
        case (e, AppCtx(e1)) =>
          App(e, e1)
        case _ => throw new IllegalStateException("Can never happen")
      }

      tmpCtx match {
        case Nil =>
          Left(tmpExpr) // as observable
        case CaseCtx(cases) :: xs =>
          Right((Case(tmpExpr, cases), xs))
        case _ => throw new IllegalStateException("No match")
      }
    }

    expr match {
      case v@BVar(_) => varHelper(v, ctx)
      case cv@ConfVar(_) => varHelper(cv, ctx)
      case gv@GlobalVar(_) => varHelper(gv, ctx)
      case l@Lambda(_) =>
        ctx match {
          case AppCtx(e1) :: xs => Right((App(l, e1), xs))
          case Nil => Left(l)
          case _ => throw new IllegalStateException("lambda in bad context")
        }
      case c@Constr(_, _) =>
        ctx match {
          case CaseCtx(cases) :: xs => Right((Case(c, cases), xs))
          case Nil => Left(c)
          case _ => throw new IllegalStateException("constructor in bad context")
        }
      case f@Fun(_) => Right((f, ctx))
      case App(e1, e2) =>
        normalize(e1, AppCtx(e2) :: ctx)
      case Case(selector, cases) =>
        normalize(selector, CaseCtx(cases) :: ctx)
      case let@Let(_, _) =>
        Right((let, ctx))
      case _ => throw new IllegalStateException("no match")
    }
  }

}
