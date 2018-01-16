package ru.mit.supercompilation

import ru.mit.supercompilation.Types._

object HomEmbedding {

  private[this] def isEmbeddedVar(expr1: Expr, expr2: Expr): Boolean = {
    (expr1, expr2) match {
      case (_: Var, _: Var) => true
      case (Fun(name1), Fun(name2)) => name1 == name2
      case _ => false
    }
  }

  private[this] def isCoupling(expr1: Expr, expr2: Expr): Boolean = {
    (expr1, expr2) match {
      case (Constr(name1, es1), Constr(name2, es2)) if name1 == name2 =>
        if (es1.size != es2.size) {
          throw new IllegalArgumentException("Same constructor has different number of parameters")
        }
        es1.zip(es2).count(e => !isEmbedded(e._1, e._2)) == 0
      case (Lambda(e1), Lambda(e2)) => isEmbedded(e1, e2)
      case (App(e11, e12), App(e21, e22)) => isEmbedded(e11, e21) && isEmbedded(e12, e22)
      case (Case(selector1, cases1), Case(selector2, cases2)) =>
        val sortedCases1 = cases1.sortBy(_._1)
        val sortedCases2 = cases2.sortBy(_._1)
        val constructors1 = sortedCases1.map(c => (c._1, c._2))
        val constructors2 = sortedCases2.map(c => (c._1, c._2))
        if (constructors1 != constructors2) {
          return false
        }
        val zippedExprs = cases1.map(_._3).zip(cases2.map(_._3))
        val notEmbeddedCnt = zippedExprs.count(e => !isEmbedded(e._1, e._2))
        isEmbedded(selector1, selector2) && notEmbeddedCnt == 0
      case _ => false
    }
  }

  private[this] def isDiving(expr1: Expr, expr2: Expr): Boolean = {
    expr2 match {
      case Constr(_, es) => es.exists(e => isEmbedded(expr1, e))
      case Lambda(e) => isEmbedded(expr1, e)
      case App(e1, e2) => isEmbedded(expr1, e1) || isEmbedded(expr1, e2)
      case Case(selector, cases) => isEmbedded(expr1, selector) || cases.map(_._3).exists(e => isEmbedded(expr1, e))
      case _ => false
    }
  }

  def isEmbedded(expr1: Expr, expr2: Expr): Boolean = {
    isEmbeddedVar(expr1, expr2) || isCoupling(expr1, expr2) || isDiving(expr1, expr2)
  }

}
