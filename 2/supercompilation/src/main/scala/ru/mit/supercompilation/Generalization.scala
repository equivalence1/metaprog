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
      case (v@GlobalVar(n), GlobalVar(m)) if n == m => (v, Nil, Nil)
      case (v@ConfVar(n), ConfVar(m)) if n == m => (v, Nil, Nil)

      case (App(e11, e12), App(e21, e22)) =>
        val res1 = commonFunctorRule(e11, e21)
        val res2 = commonFunctorRule(e12, e22)
        (App(res1._1, res2._1), res1._2 ++ res2._2, res1._3 ++ res2._3)

      case (Lambda(le1), Lambda(le2)) =>
        val innerG = commonFunctorRule(le1, le2)
        (Lambda(innerG._1), innerG._2, innerG._3)

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
