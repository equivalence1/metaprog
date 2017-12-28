package ru.mit.supercompilation

import ru.mit.supercompilation.Types._

object Generalization {

  type Generalization = (Expr, Substitution, Substitution)

  def compactGeneralization(e1: Expr, e2: Expr): Generalization = {
    val commonFunctor = commonFunctorRule(e1, e2)
    commonSubExprRule((commonFunctor._1, commonFunctor._2.sortBy(_._1), commonFunctor._3.sortBy(_._1)))
  }

  private def commonFunctorRule(e1: Expr, e2: Expr): Generalization = {
    (e1, e2) match {
      case (v@Var(n), Var(m)) if n == m => (v, Nil, Nil)
      case (v@GlobalVar(name1), GlobalVar(name2)) if name1.equals(name2) => (v, Nil, Nil)
      case (v@ConfVar(n), ConfVar(m)) if n == m => (v, Nil, Nil)
      case (f@Fun(name1), Fun(name2)) if name1 == name2 => (f, Nil, Nil)

      case (Lambda(le1), Lambda(le2)) =>
        val innerG = commonFunctorRule(le1, le2)
        (Lambda(innerG._1), innerG._2, innerG._3)

      case (App(e11, e12), App(e21, e22)) =>
        val res1 = commonFunctorRule(e11, e21)
        val res2 = commonFunctorRule(e12, e22)
        (App(res1._1, res2._1), res1._2 ++ res2._2, res1._3 ++ res2._3)

      case (Let(e11, e12), Let(e21, e22)) =>
        val res1 = commonFunctorRule(e11, e21)
        val res2 = commonFunctorRule(e12, e22)
        (Let(res1._1, res2._1), res1._2 ++ res2._2, res1._3 ++ res2._3)

      case (Constr(name1, es1), Constr(name2, es2)) if name1 == name2 =>
        val esGeneralizations = es1.zip(es2).map(e => commonFunctorRule(e._1, e._2))
        val newEs = esGeneralizations.map(_._1)
        val newSubst1 = esGeneralizations.flatMap(_._2)
        val newSubst2 = esGeneralizations.flatMap(_._3)
        (Constr(name1, newEs), newSubst1, newSubst2)

      case (Case(selector1, cases1), Case(selector2, cases2))
          if cases1.map(_._1).sorted.equals(cases2.map(_._1).sorted) =>
        val sortedCases1 = cases1.sortBy(_._1)
        val sortedCases2 = cases2.sortBy(_._1)
        val selectorsG = commonFunctorRule(selector1, selector2)
        val casesG = sortedCases1.zip(sortedCases2).map(_case => (_case._1._1, _case._1._2,
            commonFunctorRule(_case._1._3, _case._2._3)))

        val genSelector = selectorsG._1
        val genCases = casesG.map(gcase => (gcase._1, gcase._2, gcase._3._1))

        val genExpr = Case(genSelector, genCases)
        val subst1 =  casesG.flatMap(gcase => gcase._3._2)
        val subst2 =  casesG.flatMap(gcase => gcase._3._3)

        (genExpr, subst1, subst2)

      case _ =>
        val cf = ConfVar(nextFreeIndex())
        (cf, List((cf, e1)), List((cf, e2)))
    }
  }

  private def commonSubExprRule(r: Generalization): Generalization = {
    r match {
      case g@(_, Nil, Nil) => g
      case (e, (s1 :: subst1), (s2 :: subst2)) if s1._1 == s2._1 =>
        val innerG = commonSubExprRule(e, subst1, subst2)
        haveCommonSubst(innerG._2, s1._2, innerG._3, s2._2) match {
          case Some(cf) => (subst(innerG._1, List((s1._1, cf))), innerG._2, innerG._3)
          case None => (innerG._1, s1 :: innerG._2, s2 :: innerG._3)
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
