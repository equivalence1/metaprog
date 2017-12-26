package ru.mit

import ru.mit.supercompilation.Types.{Expr, Program}
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

  def ExprToString(expr: Expr): String = {
    progToString((expr, Nil))
  }

  def subst(origE: Expr, substE: Expr): Expr = {
    Subst.subst(origE, substE)
  }

  def shift(k: Int, e: Expr): Expr = {
    Subst.shift(k, e)
  }

}
