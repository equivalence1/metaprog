package ru.mit.supercompilation.printer

import org.scalatest.FunSuite
import ru.mit.supercompilation.Types.{App, Lambda, Var}

class ExprPrinterTest extends FunSuite {
  test("ExprPrinter: lambda-expr") {
    assertResult("(\\x0 . (\\x1 . x1 x0) x0) (\\x0 . x0)") {
      ExprPrinter(App(Lambda(App(Lambda(App(Var(0), Var(1))), Var(0))), Lambda(Var(0))))
    }
  }
}
