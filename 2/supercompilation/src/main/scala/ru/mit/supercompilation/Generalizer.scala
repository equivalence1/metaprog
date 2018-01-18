package ru.mit.supercompilation

import ru.mit.supercompilation.Types._

object Generalizer {

  def compactGeneralization(e1: Expr, e2: Expr): Generalization = {
    val commonFunctor = commonFunctorRule(e1, e2)
    val sortedSubts1 = commonFunctor.subst1.sortBy(_._1)
    val sortedSubts2 = commonFunctor.subst2.sortBy(_._1)
    commonSubExprRule((commonFunctor.gExpr, sortedSubts1, sortedSubts2))
  }

  private def commonFunctorRule(e1: Expr, e2: Expr): Generalization = {
    (e1, e2) match {
      case (v@BVar(n), BVar(m)) if n == m => (v, Nil, Nil)
      case (v@GlobalVar(name1), GlobalVar(name2)) if name1 == name2 => (v, Nil, Nil)
      case (v@ConfVar(n), ConfVar(m)) if n == m => (v, Nil, Nil)
      case (f@Fun(name1), Fun(name2)) if name1 == name2 => (f, Nil, Nil)

      case (Lambda(le1), Lambda(le2)) =>
        val innerG = commonFunctorRule(le1, le2)
        (Lambda(innerG.gExpr), innerG.subst1, innerG.subst2)

      case (App(e11, e12), App(e21, e22)) =>
        val res1 = commonFunctorRule(e11, e21)
        val res2 = commonFunctorRule(e12, e22)
        (App(res1.gExpr, res2.gExpr), res1.subst1 ++ res2.subst1, res1.subst2 ++ res2.subst2)

      case (Constr(name1, es1), Constr(name2, es2)) if name1 == name2 =>
        val esGeneralizations = es1.zip(es2).map(e => commonFunctorRule(e._1, e._2))
        val newEs = esGeneralizations.map(_.gExpr)
        val newSubst1 = esGeneralizations.flatMap(_.subst1)
        val newSubst2 = esGeneralizations.flatMap(_.subst2)
        (Constr(name1, newEs), newSubst1, newSubst2)

      case (Case(selector1, cases1), Case(selector2, cases2))
          if cases1.map(_.constrName).sorted == cases2.map(_.constrName).sorted =>
        val sortedCases1 = cases1.sortBy(_.constrName)
        val sortedCases2 = cases2.sortBy(_.constrName)
        val selectorsG = commonFunctorRule(selector1, selector2)
        val casesG = sortedCases1.zip(sortedCases2).map { _case =>
          (_case._1.constrName, _case._1.nrArgs, commonFunctorRule(_case._1.expr, _case._2.expr))
        }

        val genSelector = selectorsG.gExpr
        val genCases = casesG.map(gcase => CaseBranch(gcase._1, gcase._2, gcase._3.gExpr))

        val genExpr = Case(genSelector, genCases)
        val subst1 = selectorsG.subst1 ++ casesG.flatMap(gcase => gcase._3.subst1)
        val subst2 = selectorsG.subst2 ++ casesG.flatMap(gcase => gcase._3.subst2)

        (genExpr, subst1, subst2)

      case _ =>
        val cf = ConfVar(nextFreeIndex())
        (cf, List((cf, e1)), List((cf, e2)))
    }
  }

  private def commonSubExprRule(r: Generalization): Generalization = {
    r match {
      case g@Generalization(_, Nil, Nil) => g
      case Generalization(e, (s1 :: subst1), (s2 :: subst2)) if s1._1 == s2._1 =>
        val innerG = commonSubExprRule(e, subst1, subst2)
        haveCommonSubst(innerG.subst1, s1._2, innerG.subst2, s2._2) match {
          case Some(cf) => (subst(innerG.gExpr, List((s1._1, cf))), innerG.subst1, innerG.subst2)
          case None => (innerG.gExpr, s1 :: innerG.subst1, s2 :: innerG.subst2)
        }
      // impossible to reach
      case _ => throw new IllegalStateException("subst lists must have the same conf variables")
    }
  }

  private def haveCommonSubst(sub: Substitution, e1: Expr, sub1: Substitution, e2: Expr): Option[ConfVar] = {
    (sub, sub1) match {
      case (Nil, Nil) => None
      case (s1 :: subs1, s2 :: subs2) =>
        if (s1._1 == s2._1 && s1._2 == e1 && s2._2 == e2)
          Some(s1._1)
        else
          haveCommonSubst(subs1, e1, subs2, e2)
      case _ => None
    }
  }

  def apply(e1: Expr, e2: Expr): Generalization = {
    compactGeneralization(e1, e2)
  }

}
