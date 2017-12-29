package ru.mit.supercompilation.reducer

import ru.mit.supercompilation.Types._
import ru.mit.supercompilation._
import ru.mit.supercompilation.reducer.Types._

object Reducer {

    // normal expressions

//  TODO I'm not sure if I need reduce for simple expressions
  def exprReductionStep(expr: Expr, ctx: Context, fdefs: List[(String, Expr)]): NormalizedExpr = {
    expr match {
      case v@BVar(_) => (v, ctx)
      case cv@ConfVar(_) => (cv, ctx)
      case gv@GlobalVar(_) => (gv, ctx)
      case c@Constr(name, es) =>
        ctx match {
          case CaseCtx(cases) :: xs =>
            val _case = cases.find(_._1 == name).get
            val newExpr: Expr = 0.until(_case._2).foldLeft(_case._3) { (e: Expr, id: Int) =>
              shift(-1, substTo(id, e, shift(1, es(id))))
            }
            (newExpr, xs)
          case _ => (c, ctx)
        }
      case Fun(name) => (unfold(name, fdefs), ctx)
      case Lambda(e) =>
        ctx match {
          case (AppCtx(e1)) :: xs => (shift(-1, subst(e, shift(1, e1))), xs)
          case Nil => (Lambda(e), ctx)
          case _ => throw new IllegalStateException("lambda in bad place")
        }
      case App(e1, e2) => (e1, AppCtx(e2) :: ctx)
      case Case(selector, cases) => (selector, CaseCtx(cases) :: ctx)
      case Let(_, _) => throw new UnsupportedOperationException
    }
  }

  def exprReduce(expr: Expr, ctx: Context, fdefs: List[(String, Expr)]): NormalizedExpr = {
    var oldExpr: NormalizedExpr = null
    var newExpr: NormalizedExpr = (expr, Nil)
    while (!newExpr.equals(oldExpr)) {
      oldExpr = newExpr
      newExpr = exprReductionStep(newExpr._1, newExpr._2, fdefs)
    }
    newExpr
  }


  // normalized expressions

  /**
    * reduction step:
    *
    * C {ei}                                 -> C {ei}
    * \v -> e                                -> \v -> e
    * con[f]                                 -> con[unfold(f)]
    * con[(\v -> e0) e1]                     -> con[e0{v := e1}]
    * con[case Cj {e'k} of {Ci {vik} -> ei}] -> con[ej{{vjk} := {e'k}}]
    */
  def nReduceStep(nExpr: NormalizedExpr, fdefs: List[(String, Expr)]): (Boolean, NormalizedExpr) = {
    nExpr._1 match {
      case Constr(_, _) => (true, nExpr)
      case Lambda(_) => (true, nExpr)
      case Fun(name) =>
        (true, normalize(unfold(name, fdefs), nExpr._2))
      case App(Lambda(e1), e2) =>
        (true, normalize(shift(-1, subst(e1, shift(1, e2))), nExpr._2))
      case Case(Constr(name, es), cases) =>
        val _case = cases.find(_._1 == name).get
        val newExpr: Expr = 0.until(_case._2).foldLeft(_case._3) { (e: Expr, id: Int) =>
          shift(-1, substTo(id, e, shift(1, es(id))))
        }
        (true, normalize(newExpr, nExpr._2))
      case Let(_, _) => throw new UnsupportedOperationException
      case _ => (false, nExpr) // dynamic expr
    }
  }

  // This function is mostly for testing purposes right now.
  // But who knows, it might be useful later
  def nReduce(nExpr: NormalizedExpr, fdefs: List[(String, Expr)]): (Boolean, NormalizedExpr) = {
    var oldResult: NormalizedExpr = null
    var newResult = nExpr
    while (!newResult.equals(oldResult)) {
      oldResult = newResult
      val res = nReduceStep(oldResult, fdefs)
      if (!res._1)
        return res
      newResult = res._2
    }
    (true, newResult)
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
    def varHelper(expr: Expr, ctx: Context): NormalizedExpr = {
      val apps = ctx.takeWhile(_.isInstanceOf[AppCtx])
      val tmpCtx = ctx.drop(apps.size)
      val tmpExpr = apps.foldLeft(expr) {
        case (e, AppCtx(e1)) =>
          App(e, e1)
        case _ => throw new IllegalStateException("Can never happen")
      }

      tmpCtx match {
        case Nil =>
          (tmpExpr, Nil) // as observable
        case CaseCtx(cases) :: xs =>
          (Case(tmpExpr, cases), xs) // as redex
        case _ => throw new IllegalStateException("No match")
      }
    }

    expr match {
      case v@BVar(_) => varHelper(v, ctx)
      case cv@ConfVar(_) => varHelper(cv, ctx)
      case gv@GlobalVar(_) => varHelper(gv, ctx) // TODO
      case l@Lambda(_) =>
        ctx match {
          case AppCtx(e1) :: xs => (App(l, e1), xs)
          case Nil => (l, ctx)
          case _ => throw new IllegalStateException("lambda in bad context")
        }
      case c@Constr(_, _) =>
        ctx match {
          case CaseCtx(cases) :: xs => (Case(c, cases), xs)
          case Nil => (c, ctx)
          case _ => throw new IllegalStateException("constructor in bad context")
        }
      case f@Fun(_) => (f, ctx)
      case App(e1, e2) =>
        normalize(e1, AppCtx(e2) :: ctx)
      case Case(selector, cases) =>
        normalize(selector, CaseCtx(cases) :: ctx)
      case Let(_, _) => throw new UnsupportedOperationException
      case _ => throw new IllegalStateException("no match")
    }
  }

}
