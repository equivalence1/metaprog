package ru.mit

import ru.mit.supercompilation.Types._
import ru.mit.supercompilation.parser.{AstTransformer, Parser}
import ru.mit.supercompilation.printer.ProgPrinter
import ru.mit.supercompilation.reducer.Reducer
import ru.mit.supercompilation.reducer.Types.{AppCtx, CaseCtx, Context, NormalizedExpr}

package object supercompilation {

  def parseProg(code: String): Program = {
    AstTransformer(Parser(code))
  }

  def parseExpr(code: String): Expr = {
    parseProg(code)._1
  }

  def progToString(prog: Program): String = {
    ProgPrinter(prog)
  }

  def exprToString(expr: Expr): String = {
    progToString((expr, Nil))
  }

  def substTo(index: Int, origE: Expr, substE: Expr): Expr = {
    Subst.substTo(index, origE, substE)
  }

  def subst(origE: Expr, substE: Expr): Expr = {
    substTo(0, origE, substE)
  }

  def subst(origE: Expr, substitution: Substitution): Expr = {
    Subst.subst(origE, substitution)
  }

  def shift(k: Int, e: Expr): Expr = {
    Subst.shift(k, e)
  }

  def unfold(fName: String, fdefs: List[(String, Expr)]): Expr = {
    fdefs.find(fDef => fDef._1 == fName).get._2
  }

  def generalize(e1: Expr, e2: Expr): Generalization.Generalization = {
    Generalization(e1, e2)
  }

  def normalize(e: Expr): NormalizedExpr = {
    Reducer.normalize(e, Nil)
  }

  def normalize(e: Expr, ctx: Context): NormalizedExpr = {
    Reducer.normalize(e, ctx)
  }

  def toExpr(nExpr: NormalizedExpr): Expr = {
    nExpr match {
      case Left(e) => e
      case Right((e, ctx)) =>
        ctx.foldLeft(e) { (e, ctxStep) =>
          ctxStep match {
            case AppCtx(e1) => App(e, e1)
            case CaseCtx(cases) => Case(e, cases)
          }
        }
    }
  }

  private def containsConf(expr: Types.Expr, id: Int): Boolean = {
    expr match {
      case ConfVar(cvId) => id == cvId
      case Lambda(e) => containsConf(e, id)
      case App(e1, e2) => containsConf(e1, id) || containsConf(e2, id)
      case Let(subst, e) => subst.exists(s => containsConf(s._2, id)) || containsConf(e, id)
      case Constr(_, es) => es.exists(e => containsConf(e, id))
      case Case(selector, cases) => cases.exists(c => containsConf(c._3, id)) || containsConf(selector, id)
      case _ => false
    }
  }

  def isInstance(expr1: Expr, expr2: Expr): Option[Substitution] = {
    (expr1, expr2) match {
//      case (BVar(id1), BVar(id2)) if id1 == id2 =>
//        Some(Nil)
      case (BVar(_), BVar(_)) =>
        None
      case (ConfVar(id1), ConfVar(id2)) if id1 == id2 =>
        Some(Nil)
      case (GlobalVar(name1), GlobalVar(name2)) if name1 == name2 =>
        Some(Nil)
      case (Fun(name1), Fun(name2)) if name1 == name2 =>
        Some(Nil)
      case (e@_, cv@ConfVar(id2)) =>
        if (containsConf(e, id2)) {
          None
        } else {
          Some(List((cv, e)))
        }
      case (Lambda(e1), Lambda(e2)) =>
        isInstance(e1, e2)
      case (Let(subst1, e1), Let(subst2, e2)) =>
        val newE1 = subst(e1, subst1)
        val newE2 = subst(e2, subst2)
        isInstance(newE1, newE2)
      case (App(e11, e12), App(e21, e22)) =>
        val instance1 = isInstance(e11, e21)
        val instance2 = isInstance(e12, e22)
        (instance1, instance2) match {
          case (Some(subst1), Some(subst2)) => Some(subst1 ++ subst2)
          case _ => None
        }
      case (Constr(name1, es1), Constr(name2, es2)) =>
        if (name1.equals(name2)) {
          val zippedArgs = es1.zip(es2)
          var res: Substitution = Nil
          for ((e1, e2) <- zippedArgs) {
            isInstance(e1, e2) match {
              case Some(subst) => res = res ++ subst
              case None => return None
            }
          }
          Some(res)
        } else {
          None
        }
      case (Case(selector1, cases1), Case(selector2, cases2)) =>
        val selectorsInstance = isInstance(selector1, selector2)
        if (selectorsInstance.isEmpty) {
          return None
        }
        val sortedCases1 = cases1.sortBy(_._1)
        val sortedCases2 = cases2.sortBy(_._1)
        if (!sortedCases1.map(c => (c._1, c._2)).equals(sortedCases2.map(c => (c._1, c._2)))) {
          return None
        }
        val zippedCases = cases1.map(_._3).zip(cases2.map(_._3))
        var casesRes: Substitution = Nil
        for ((e1, e2) <- zippedCases) {
          isInstance(e1, e2) match {
            case Some(subst) => casesRes = casesRes ++ subst
            case None => return None
          }
        }
        Some(selectorsInstance.get ++ casesRes)
      case _ => None
    }
  }

  def isEmbedded(e1: Expr, e2: Expr): Boolean = {
    HomEmbedding.isEmbedded(e1, e2)
  }

  def getExprClass(e: NormalizedExpr): Int = {
    e match {
      case Right((Let(_, _), _)) =>
        0
      case Left(v) if v.isInstanceOf[Var] =>
        0
      case Left(a@App(_, _)) =>
        val fApp = flattenApp(a)
        if (!fApp.head.isInstanceOf[Var]) {
          throw new IllegalArgumentException("Node expression is not normalized")
        }
        0
      case Left(Constr(_, _)) =>
        0
      case Left(Lambda(_)) =>
        0
      case Right((App(Lambda(_), _), _)) =>
        1
      case Right((Fun(_), _)) =>
        2
      case Right((Case(Constr(_, _), _), _)) =>
        3
      case Right((Case(_, _), _)) =>
        4
      case _ => throw new IllegalArgumentException("Node expression is not normalized")
    }
  }

  def isTrivial(e: NormalizedExpr): Boolean = {
    getExprClass(e) == 0
  }

  def isBettaTrans(e: NormalizedExpr): Boolean = {
    getExprClass(e) == 1
  }

  def isGlobal(e: NormalizedExpr): Boolean = {
    getExprClass(e) == 4
  }

  def isLocal(e: NormalizedExpr): Boolean = {
    !isGlobal(e)
  }

  def isCycleCandidate(e: NormalizedExpr): Boolean = {
    getExprClass(e) > 1
  }

  // TODO: bad that global -- affects testing, need to invalidate each time
  private[supercompilation] var nextId: Int = -1
  def nextFreeIndex(): Int = {
    nextId += 1
    nextId
  }

  // TODO same
  private[supercompilation] var nextFunId: Int = -1
  def nextFreeFunIndex(): Int = {
    nextFunId += 1
    nextFunId
  }

  def flattenApp(a: App): List[Expr] = {
    a.e1 match {
      case e1App@App(_, _) =>
        flattenApp(e1App) ++ List(a.e2)
      case _ =>
        List(a.e1, a.e2)
    }
  }

}
