package ru.mit.supercompilation.printer

import ru.mit.supercompilation.Types._

object ProgPrinter {

  var sb: StringBuilder = _
  var funcs: List[String] = _

  // TODO: to many ()
  private def print(e: Expr, namesStack: List[String]): Unit = {
    e match {
      case BVar(n) =>
        if (n >= namesStack.size)
          sb.append("OOOOPS")
        else
          sb.append(namesStack(n))

      case ConfVar(id) =>
        sb.append("y" + id)

      case GlobalVar(name) =>
        sb.append(name)

      case Lambda(le) =>
        val varName = "x" + namesStack.size
        sb.append("(\\" + varName + " . ")
        print(le, varName :: namesStack)
        sb.append(")")

      case App(e1, e2) =>
        sb.append("(")
        print(e1, namesStack)
        sb.append(" ")
        print(e2, namesStack)
        sb.append(")")

      case Let(e1, e2) =>
        val varName = "x" + namesStack.size
        sb.append("let ")
        sb.append(varName)
        sb.append(" = ")
        //print(e1, namesStack) // don't change namesStack here!
        sb.append(" in ")
        print(e2, varName :: namesStack)

      case Fun(name) =>
        sb.append(name)

      case Constr(name, es) =>
        sb.append(name)
        es.foreach(e => {sb.append(" "); print(e, namesStack)})

      case Case(selector, cases) =>
        sb.append("case ")
        print(selector, namesStack)
        sb.append(" of {")
        var first = true
        for ((constrName, varsNr, caseExpr) <- cases) {
          if (!first) {
            sb.append(" | ")
          }
          first = false
          sb.append(constrName)

          var newNamesStack = namesStack
          0.until(varsNr).foreach { id =>
            val varName = "x" + (id + namesStack.size)
            sb.append(" ")
            sb.append(varName)
            newNamesStack = varName :: newNamesStack
          }

          sb.append(" -> ")
          print(caseExpr, newNamesStack)
        }
        sb.append("}")
    }
  }

//  private def restoreExpr(rExpr: ReducedExpr): Expr = {
//    rExpr match {
//      case (e, Nil) => e
//      case (e, LambdaCtx :: ctx) => restoreExpr(Lambda(e), ctx)
//      case (e, (AppCtx(e1)) :: ctx) => restoreExpr(App(e, e1), ctx)
//    }
//  }

  def apply(e: Expr): String = {
    sb = new StringBuilder
    print(e, Nil)
    sb.toString()
  }

//  def apply(rExpr: ReducedExpr): String = {
//    ProgPrinter(restoreExpr(rExpr))
//  }

  def apply(prog: Program): String = {
    funcs = prog._2.map(d => d._1)

    val result: StringBuilder = new StringBuilder
    result.append(apply(prog._1))
    if (funcs.nonEmpty) {
      result.append("\n  where")
      for ((funName, funExpr) <- prog._2) {
        result.append("\n    ")
        result.append(funName)
        result.append(" = ")
        result.append(apply(funExpr))
        result.append(";")
      }
    }

    result.toString()
  }

}
