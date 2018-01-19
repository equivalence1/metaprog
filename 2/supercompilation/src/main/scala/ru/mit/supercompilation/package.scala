package ru.mit

import ru.mit.supercompilation.Types._
import ru.mit.supercompilation.parser.{ExprTranslator, Parser}
import ru.mit.supercompilation.printer.ProgPrinter

package object supercompilation {

  def parseProg(code: String): Program = {
    ExprTranslator(Parser(code))
  }

  // mostly for tests
  def parseExpr(code: String): Expr = {
    parseProg(code).mainExpr
  }

  def progToString(prog: Program): String = {
    ProgPrinter(prog)
  }

  def exprToString(expr: Expr): String = {
    progToString(Program(expr, Nil))
  }

  def subst(origE: Expr, substE: Expr): Expr = {
    Subst.subst(origE, substE)
  }

  def subst(origE: Expr, substitution: Substitution): Expr = {
    Subst.subst(origE, substitution)
  }

  def shift(k: Int, e: Expr): Expr = {
    Subst.shift(k, e)
  }

  def unfold(fName: String, fdefs: List[FDef]): Expr = {
    fdefs.find(fDef => fDef.fName == fName).get.body
  }

  def generalize(e1: Expr, e2: Expr): Generalization = {
    Generalizer(e1, e2)
  }

  def normalize(e: Expr): NormalizedExpr = {
    Reducer.normalize(e, Nil)
  }

  def normalize(e: Expr, ctx: Context): NormalizedExpr = {
    Reducer.normalize(e, ctx)
  }

  def replaceUnbound(expr: Expr, lvl: Int): Expr = {
    expr match {
      case v@BVar(id) =>
        if (id > lvl) {
          ConfVar(lvl - id)
        } else {
          v
        }
      case v: Var => v
      case f@Fun(_) => f
      case Lambda(e) => Lambda(replaceUnbound(e, lvl + 1))
      case App(e1, e2) => App(replaceUnbound(e1, lvl), replaceUnbound(e2, lvl))
      case Let(s, e) => Let(s, replaceUnbound(e, lvl)) // TODO should i do something with s?
      case c@Constr(_, _) => c
      case Case(selector, cases) =>
        val newCases = cases.map(c => CaseBranch(c.constrName, c.nrArgs, replaceUnbound(c.expr, lvl + c.nrArgs)))
        Case(replaceUnbound(selector, lvl), newCases)
    }
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

  def isInstance(expr1: Expr, expr2: Expr): Option[Substitution] = {
    def isInstance(expr1: Expr, expr2: Expr, lvl: Int): Option[Substitution] = {
      (expr1, expr2) match {
        case (BVar(id1), BVar(id2)) if id1 == id2 && id1 <= lvl =>
          Some(Nil)
        case (e@_, BVar(id2)) if id2 > lvl =>
          Some(List((ConfVar(lvl - id2), e)))
        case (ConfVar(id1), ConfVar(id2)) if id1 == id2 =>
          Some(Nil)
        case (GlobalVar(name1), GlobalVar(name2)) if name1 == name2 =>
          Some(Nil)
        case (Fun(name1), Fun(name2)) if name1 == name2 =>
          Some(Nil)
        case (e@_, cv@ConfVar(_)) =>
          Some(List((cv, e)))
        case (Lambda(e1), Lambda(e2)) =>
          isInstance(e1, e2, lvl + 1)
        case (Let(subst1, e1), Let(subst2, e2)) =>
          val newE1 = subst(e1, subst1)
          val newE2 = subst(e2, subst2)
          isInstance(newE1, newE2, lvl)
        case (App(e11, e12), App(e21, e22)) =>
          val instance1 = isInstance(e11, e21, lvl)
          val instance2 = isInstance(e12, e22, lvl)
          (instance1, instance2) match {
            case (Some(subst1), Some(subst2)) => Some(subst1 ++ subst2)
            case _ => None
          }
        case (Constr(name1, es1), Constr(name2, es2)) =>
          if (name1.equals(name2)) {
            val zippedArgs = es1.zip(es2)
            var res: Substitution = Nil
            for ((e1, e2) <- zippedArgs) {
              isInstance(e1, e2, lvl) match {
                case Some(subst) => res = res ++ subst
                case None => return None
              }
            }
            Some(res)
          } else {
            None
          }
        case (c1@Case(selector1, cases1), c2@Case(selector2, cases2)) if c1.hasSameBranches(c2) =>
          val selectorsInstance = isInstance(selector1, selector2, lvl)
          if (selectorsInstance.isEmpty) {
            return None
          }
          val zippedCases = cases1.map(c => (c.nrArgs, c.expr)).zip(cases2.map(_.expr))
          var casesRes: Substitution = Nil
          for (((nrArgs, e1), e2) <- zippedCases) {
            isInstance(e1, e2, lvl + nrArgs) match {
              case Some(subst) => casesRes = casesRes ++ subst
              case None => return None
            }
          }
          Some(selectorsInstance.get ++ casesRes)
        case _ => None
      }
    }
    isInstance(expr1, expr2, -1)
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

  def nextFreeIndex(): Int = {
    Supercompiler.nextFreeIndex()
  }

  def nextFreeFunIndex(): Int = {
    Supercompiler.nextFreeFunIndex()
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
