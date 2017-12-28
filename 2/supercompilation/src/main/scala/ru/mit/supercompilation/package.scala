package ru.mit

import ru.mit.supercompilation.Types._
import ru.mit.supercompilation.parser.{AstTransformer, Parser}
import ru.mit.supercompilation.printer.ProgPrinter

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

  def subst(origE: Expr, substE: Expr): Expr = {
    Subst.subst(origE, substE)
  }

  def subst(origE: Expr, substitution: Substitution): Expr = {
    Subst.subst(origE, substitution)
  }

  def shift(k: Int, e: Expr): Expr = {
    Subst.shift(k, e)
  }

  def unfold(fName: String, prog: Program): Expr = {
    prog._2.find(fDef => fDef._1 == fName).get._2
  }

  def generalize(e1: Expr, e2: Expr): Generalization.Generalization = {
    Generalization(e1, e2)
  }

  def isClosure(e: Expr): Boolean = {
    def isClosure(e: Expr, n: Int): Boolean = {
      e match {
        case Var(v) => if (v >= n) false else true
        case ConfVar(_) => true
        case GlobalVar(_) => true
        case Fun(_) => true
        case Lambda(e1) => isClosure(e1, n + 1)
        case App(e1, e2) => isClosure(e1, n) && isClosure(e2, n)
        case Let(e1, e2) => isClosure(e1, n) && isClosure(e2, n + 1)
        case Constr(_, es) => es.count(e => isClosure(e, n)) == es.size
        case Case(selector, cases) => isClosure(selector, n) &&
          cases.count(c => isClosure(c._3, n + c._2)) == cases.size
      }
    }
    isClosure(e, 0)
  }

  // TODO: bad that global -- affects testing, need to invalidate each time
  private[supercompilation] var nextId: Int = -1

  def nextFreeIndex(/*e: Expr*/): Int = {
//    e match {
//      case Var(_) => 0
//      case ConfVar(id) => id + 1
//      case GlobalVar(_) => 0
//      case Fun(_) => 0
//      case Lambda(e) => nextFreeIndex(e)
//      case App(e1, e2) => Math.max(nextFreeIndex(e1), nextFreeIndex(e2))
//      case Let(e1, e2) => Math.max(nextFreeIndex(e1), nextFreeIndex(e2))
//      case Constr(_, es) => nextFreeIndex(es.maxBy(e => nextFreeIndex(e)))
//      case Case(selector, cases) => Math.max(nextFreeIndex(selector),
//        nextFreeIndex(cases.maxBy(c => nextFreeIndex(c._3))._3))
//    }

    nextId += 1
    nextId
  }

}
