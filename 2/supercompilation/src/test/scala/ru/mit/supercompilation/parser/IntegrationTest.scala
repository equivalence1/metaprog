package ru.mit.supercompilation.parser

import org.scalatest.FunSuite
import ru.mit.supercompilation.Types._

class IntegrationTest extends FunSuite {
  test("Lexer + Parser + Builder: lambda-expr") {
    assertResult(App(Lambda(App(Lambda(App(Var(0), Var(1))), Var(0))), Lambda(Var(0)))) {
      //ProgramAstTransformer(Parser("(\\x . (\\y . y x) x) (\\x . x)"))
    }
  }
}
