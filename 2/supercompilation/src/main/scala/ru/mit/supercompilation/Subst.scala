package ru.mit.supercompilation

import ru.mit.supercompilation.Types._

object Subst {

  // substitutions into bounded variable (like when applying a lambda to an expression)

  private[this] def shiftFrom(from: Int, k: Int, expr: Expr): Expr = {
    def shift(e: Expr): Expr = {
      shiftFrom(from, k, e)
    }

    expr match {
      case BVar(v) => if (v >= from) BVar(v + k) else BVar(v)
      case cf@ConfVar(_) => cf
      case gv@GlobalVar(_) => gv
      case Lambda(e) => Lambda(shiftFrom(from + 1, k, e))
      case App(e1, e2) => App(shift(e1), shift(e2))
      case Let(s, e) => Let(s.map(e => (e._1, shiftFrom(from, k, e._2))), shiftFrom(from, k, e))
      case f@Fun(_) => f
      case Constr(name, es) => Constr(name, es.map {e => shift(e)})
      case Case(selector, cases) => Case(shift(selector),
        cases.map {br => CaseBranch(br.constrName, br.nrArgs, shiftFrom(from + br.nrArgs, k, br.expr))})
    }
  }

  def shift(k: Int, expr: Expr): Expr = {
    shiftFrom(0, k, expr)
  }

  private[this] def substTo(index: Int, origE: Expr, substE: Expr): Expr = {
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
        cases.map {br => CaseBranch(br.constrName, br.nrArgs, substTo(index + br.nrArgs, br.expr, shift(br.nrArgs, substE)))})
    }
  }

  def subst(origE: Expr, substE: Expr): Expr = {
    substTo(0, origE, substE)
  }

  // substitutions of configuration variables

  private[this] def substConf(index: Int, origE: Expr, substE: Expr, lvl: Int): Expr = {
    def subst(e: Expr): Expr = {
      substConf(index, e, substE, lvl)
    }

    origE match {
      case BVar(id) =>
        if (index < 0 && lvl - id == index) {
          substE
        } else {
          BVar(id)
        }
      case ConfVar(v) => if (v == index) substE else ConfVar(v)
      case gv@GlobalVar(_) => gv
      case Lambda(e) => Lambda(substConf(index, e, substE, lvl + 1))
      case App(e1, e2) => App(subst(e1), subst(e2))
      case Let(s, e) =>
        Let(s.map(e => (e._1, subst(e._2))), subst(e))
      case f@Fun(_) => f
      case Constr(name, es) => Constr(name, es.map {e => subst(e)})
      case Case(selector, cases) => Case(subst(selector),
        cases.map {br => CaseBranch(br.constrName, br.nrArgs, substConf(index, br.expr, substE, lvl + br.nrArgs))})
    }
  }

  def subst(origE: Expr, s: Substitution): Expr = {
    s.foldLeft(origE) { (e, s) =>
      substConf(s._1.id, e, s._2, -1)
    }
  }
}
