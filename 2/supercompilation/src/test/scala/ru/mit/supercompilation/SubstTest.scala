package ru.mit.supercompilation

import org.scalatest.FunSuite
import ru.mit.supercompilation.Types._

class SubstTest extends FunSuite {
  test("lambda expr") {
    assertResult("""((\x0 -> (v x0)) (\x0 -> (\x1 -> (x0 (\x2 -> (v x2))))))""") {
      exprToString(subst(App(BVar(0), Lambda(Lambda(App(BVar(1), BVar(2))))), Lambda(App(GlobalVar("v"), BVar(0)))))
    }
  }

  test("full code") {
    val expectedResult = """(\x0 -> (\x1 -> case (Constr x1 x0 glob) of {Case1 x2 -> (\x3 -> case x3 of {A x4 -> x3}) | Case2 -> x0}))"""
    assertResult(expectedResult) {
      /*
      origE:
      \Var(0) Var(1) . case (Constr Var(1) Var(0) glob) of {
          Case1 Var(2) -> Var(3)
        | Case2 -> Var(1)
      }

      substE:
      \Var(0) -> case Var(0) of {A Var(1) -> Var(0)}
       */

      val origBranches = List(CaseBranch("Case1", 1, BVar(3)), CaseBranch("Case2", 0, BVar(1)))
      val origCase = Case(Constr("Constr", List(BVar(0), BVar(1), GlobalVar("glob"))), origBranches)
      val origE = Lambda(Lambda(origCase))

      val substBranches = List(CaseBranch("A", 1, BVar(1)))
      val substCase = Case(BVar(0), substBranches)
      val substE = Lambda(substCase)

      exprToString(subst(origE, substE))
    }
  }
}