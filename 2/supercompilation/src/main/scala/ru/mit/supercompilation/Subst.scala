package ru.mit.supercompilation

import ru.mit.supercompilation.Types._

object Subst {

  // substitutions into bounded variable (like when applying a lambda to an expression)

  private def shiftN(from: Int, k: Int, expr: Expr): Expr = {
    def shift(e: Expr): Expr = {
      shiftN(from, k, e)
    }

    expr match {
      case BVar(v) => if (v >= from) BVar(v + k) else BVar(v)
      case cf@ConfVar(_) => cf
      case gv@GlobalVar(_) => gv
      case Lambda(e) => Lambda(shiftN(from + 1, k, e))
      case App(e1, e2) => App(shift(e1), shift(e2))
      case Let(s, e) => Let(s.map(e => (e._1, shiftN(from, k, e._2))), shiftN(from, k, e))
      case f@Fun(_) => f
      case Constr(name, es) => Constr(name, es.map {e => shift(e)})
      case Case(selector, cases) => Case(shift(selector),
        cases.map {br => (br._1, br._2, shiftN(from + br._2, k, br._3))})
    }
  }

  def shift(k: Int, expr: Expr): Expr = {
    shiftN(0, k, expr)
  }

  def substTo(index: Int, origE: Expr, substE: Expr): Expr = {
    def subst(e: Expr): Expr = {
      substTo(index, e, substE)
    }

    origE match {
      case BVar(v) => if (v == index) substE else BVar(v)
      case cf@ConfVar(_) => cf
      case gv@GlobalVar(_) => gv
      case Lambda(e) => Lambda(substTo(index + 1, e, shift(1, substE)))
      case App(e1, e2) => App(subst(e1), subst(e2))
      case Let(s, e) => Let(s.map(e => (e._1, subst(e._2))), subst(e))
      case f@Fun(_) => f
      case Constr(name, es) => Constr(name, es.map {e => subst(e)})
      case Case(selector, cases) => Case(subst(selector),
        cases.map {br => (br._1, br._2, substTo(index + br._2, br._3, shift(br._2, substE)))})
    }
  }

  def subst(origE: Expr, substE: Expr): Expr = {
    substTo(0, origE, substE)
  }

  // substitutions of configuration variables

  private def substConf(index: Int, origE: Expr, substE: Expr): Expr = {
    def substConfSame(e: Expr): Expr = {
      substConf(index, e, substE)
    }

    origE match {
      case v@BVar(_) => v
      case ConfVar(v) => if (v == index) substE else ConfVar(v)
      case gv@GlobalVar(_) => gv
      case Lambda(e) => Lambda(substConf(index, e, shift(1, substE)))
      case App(e1, e2) => App(substConfSame(e1), substConfSame(e2))
      case Let(s, e) =>
//        if (s.exists(_._1.id == index)) {
//          Let(s.map(e => (e._1, substConfSame(e._2))), e)
//        } else {
          Let(s.map(e => (e._1, substConfSame(e._2))), substConfSame(e))
//        }
      case f@Fun(_) => f
      case Constr(name, es) => Constr(name, es.map {e => substConfSame(e)})
      case Case(selector, cases) => Case(substConfSame(selector),
        cases.map {br => (br._1, br._2, substConf(index, br._3, shift(br._2, substE)))})
    }
  }

  def subst(origE: Expr, s: Substitution): Expr = {
    s.foldLeft(origE) { (e, s) =>
      substConf(s._1.id, e, s._2)
    }
  }
}
