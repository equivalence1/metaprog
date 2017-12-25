package ru.mit.supercompilation.reducer

import org.scalatest.FunSuite
import ru.mit.supercompilation.Types._
import ru.mit.supercompilation.parser.Parser

class ReducerTest extends FunSuite {
  test("reducer lambda-expr 1") {
    assertResult((Lambda(Var(0)), Nil)) {
      Reducer.reduce(App(Lambda(App(Lambda(App(Var(0), Var(1))), Var(0))), Lambda(Var(0))), Nil)
    }
  }

  test("Danya's test -- reduce + restore") {
    assertResult("(\\x0 . (\\x1 . (\\x2 . (\\x3 . ((x0 x2) ((x1 x2) x3))))))") {
      val ast = Parser("(\\ t1 . ((t1 (\\ n a1 x1 . n (\\ sva2 zva2 . ((a1 sva2) ((x1 sva2) zva2))))) (\\ a . a)) (\\ s z . z)) (\\ s1 z1 . s1 (s1 z1))")
//      ExprPrinter(Reducer.reduce(ProgramAstTransformer(ast), Nil))
      null
    }
  }
}