package ru.mit.supercompilation

import ru.mit.supercompilation.Types.{App, Case, Expr}

/**
  * con ::= <>
  *       | con e
  *       | case con of {pi -> ei}
  */
sealed trait ContextStep
case class AppCtx(e: Expr) extends ContextStep
case class CaseCtx(cases: List[(String, Int, Expr)]) extends ContextStep

class Context {

  private val steps: List[ContextStep] = Nil

  def insert(e: Expr): Expr = {
    steps.foldLeft(e) { (e, step) =>
      step match {
        case AppCtx(e1) => App(e, e1)
        case CaseCtx(cases) => Case(e, cases)
      }
    }
  }

}
