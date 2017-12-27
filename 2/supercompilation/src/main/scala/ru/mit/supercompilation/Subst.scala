package ru.mit.supercompilation

import ru.mit.supercompilation.Types._

object Subst {

  // substitutions into bounded variable (like when applying a lambda to an expression)

  private def shiftN(from: Int, k: Int, expr: Expr): Expr = {
    def shift(e: Expr): Expr = {
      shiftN(from, k, e)
    }

    expr match {
      case Var(v) => if (v >= from) Var(v + k) else Var(v)
      case gv@GlobalVar(_) => gv
      case Lambda(e) => Lambda(shiftN(from + 1, k, e))
      case App(e1, e2) => App(shift(e1), shift(e2))
      case Let(e1, e2) => Let(shift(e1), shiftN(from + 1, k, e2))
      case f@Fun(_) => f
      case Constr(name, es) => Constr(name, es.map {e => shift(e)})
      case Case(selector, cases) => Case(shift(selector),
        cases.map {br => (br._1, br._2, shiftN(from + br._2, k, br._3))})
    }
  }

  def shift(k: Int, expr: Expr): Expr = {
    shiftN(0, k, expr)
  }

  private def substN(index: Int, origE: Expr, substE: Expr): Expr = {
    def subst(e: Expr): Expr = {
      substN(index, e, substE)
    }

    origE match {
      case Var(v) => if (v == index) substE else Var(v)
      case cf@ConfVar(_) => cf
      case gv@GlobalVar(_) => gv
      case Lambda(e) => Lambda(substN(index + 1, e, shift(1, substE)))
      case App(e1, e2) => App(subst(e1), subst(e2))
      case Let(e1, e2) => Let(subst(e1), substN(index + 1, e2, shift(1, substE)))
      case f@Fun(_) => f
      case Constr(name, es) => Constr(name, es.map {e => subst(e)})
      case Case(selector, cases) => Case(subst(selector),
        cases.map {br => (br._1, br._2, substN(index + br._2, br._3, shift(br._2, substE)))})
    }
  }

  def subst(origE: Expr, substE: Expr): Expr = {
    substN(0, origE, substE)
  }

  // substitutions of configuration variables

  private def substConf(index: Int, origE: Expr, substE: Expr): Expr = {
    def substConfSame(e: Expr): Expr = {
      substConf(index, e, substE)
    }

    origE match {
      case v@Var(_) => v
      case ConfVar(v) => if (v == index) substE else ConfVar(v)
      case gv@GlobalVar(_) => gv
      case Lambda(e) => Lambda(substConfSame(e))
      case App(e1, e2) => App(substConfSame(e1), substConfSame(e2))
      case Let(e1, e2) => Let(substConfSame(e1), substConfSame(e2))
      case f@Fun(_) => f
      case Constr(name, es) => Constr(name, es.map {e => substConfSame(e)})
      case Case(selector, cases) => Case(substConfSame(selector),
        cases.map {br => (br._1, br._2, substConfSame(br._3))})
    }
  }

  def subst(origE: Expr, s: Substitution): Expr = {
    s.foldLeft(origE) { (e, s) =>
      if (!isClosure(s._2)) {
        throw new IllegalArgumentException(s._2 + " is not a valid substitution")
      }
      substConf(s._1.v, e, s._2)
    }
  }
}
