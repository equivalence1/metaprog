package ru.mit

import ru.mit.supercompilation.Types._
import ru.mit.supercompilation.parser.{AstTransformer, Parser}
import ru.mit.supercompilation.printer.ProgPrinter
import ru.mit.supercompilation.reducer.Reducer
import ru.mit.supercompilation.reducer.Types.{Context, NormalizedExpr}

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

  // TODO: bad that global -- affects testing, need to invalidate each time
  private[supercompilation] var nextId: Int = -1
  def nextFreeIndex(): Int = {
    nextId += 1
    nextId
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
