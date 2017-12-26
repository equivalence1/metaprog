package ru.mit.supercompilation

import org.scalatest.FunSuite
import ru.mit.supercompilation.Types._

/**
  * When I parse expressions, I convert all top-level unbound variables into
  * global variables.
  *
  * This seems like a quite logical decision, but this means that to check
  * substitution I have to construct [[Expr]]ession manually (can't parse it from string)
  */
class SubstTest extends FunSuite {
  test("lambda expr") {
    assertResult("""((\x0 . (v x0)) (\x0 . (\x1 . (x0 (\x2 . (v x2))))))""") {
      ExprToString(subst(App(Var(0), Lambda(Lambda(App(Var(1), Var(2))))), Lambda(App(GlobalVar("v"), Var(0)))))
    }
  }

  test("full code") {
    val expectedResult = """(\x0 . (\x1 . case Constr x1 x0 glob of {Case1 x2 -> (\x3 . case x3 of {A x4 -> x3}) | Case2 -> x0}))"""
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

      val origBranches = ("Case1", 1, Var(3)) :: ("Case2", 0, Var(1)) :: Nil
      val origCase = Case(Constr("Constr", Var(0) :: Var(1) :: GlobalVar("glob") :: Nil), origBranches)
      val origE = Lambda(Lambda(origCase))

      val substBranches = ("A", 1, Var(1)) :: Nil
      val substCase = Case(Var(0), substBranches)
      val substE = Lambda(substCase)

      ExprToString(subst(origE, substE))
    }
  }
}