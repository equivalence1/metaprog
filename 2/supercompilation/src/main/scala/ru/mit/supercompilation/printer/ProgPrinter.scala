package ru.mit.supercompilation.printer

import ru.mit.supercompilation.Types._

/**
  * Well, it's not actually a printer.
  * This object only transforms [[Expr]] into [[String]],
  * but I didn't find more appropriate name
  */
object ProgPrinter {

  var sb: StringBuilder = _
  var funcs: List[String] = _

  // TODO: to many ()
  private[this] def print(e: Expr, namesStack: List[String]): Unit = {
    e match {
      case BVar(n) =>
        sb.append(namesStack(n))

      case ConfVar(id) =>
        sb.append("y" + id)

      case GlobalVar(name) =>
        sb.append(name)

      case Lambda(le) =>
        val varName = "x" + namesStack.size
        sb.append("(\\" + varName + " -> ")
        print(le, varName :: namesStack)
        sb.append(")")

      case App(e1, e2) =>
        sb.append("(")
        print(e1, namesStack)
        sb.append(" ")
        print(e2, namesStack)
        sb.append(")")

      case Let(_, _) =>
        throw new IllegalArgumentException("Let expression is inadmissible in an output expression")

      case Fun(name) =>
        sb.append(name)

      case Constr(name, es) =>
        if (es.nonEmpty) {
          sb.append("(")
        }
        sb.append(name)
        es.foreach(e => {sb.append(" "); print(e, namesStack)})
        if (es.nonEmpty) {
          sb.append(")")
        }

      case Case(selector, cases) =>
        sb.append("case ")
        print(selector, namesStack)
        sb.append(" of {")
        var first = true
        for (CaseBranch(constrName, varsNr, caseExpr) <- cases) {
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

  def apply(e: Expr): String = {
    sb = new StringBuilder
    print(e, Nil)
    sb.toString()
  }

  def apply(prog: Program): String = {
    funcs = prog.fdefs.map(d => d.fName)

    val result: StringBuilder = new StringBuilder
    result.append(apply(prog.mainExpr))
    if (funcs.nonEmpty) {
      result.append("\n  where")
      for (FDef(funName, funExpr) <- prog.fdefs) {
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
