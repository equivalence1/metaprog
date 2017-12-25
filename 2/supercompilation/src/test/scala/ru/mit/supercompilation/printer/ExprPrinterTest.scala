package ru.mit.supercompilation.printer

import org.scalatest.FunSuite
import ru.mit.supercompilation.Types.{App, Lambda, Var}
import ru.mit.supercompilation.parser.{AstTransformer, Parser}

class ExprPrinterTest extends FunSuite {
  test("lambda-expr") {
    assertResult("((\\x0 . ((\\x1 . (x1 x0)) x0)) (\\x0 . x0))") {
      ProgPrinter(App(Lambda(App(Lambda(App(Var(0), Var(1))), Var(0))), Lambda(Var(0))))
    }
  }

  test("full program") {
    assertResult(
      """((f g) Z)
        |  where
        |    f = (\x0 . (\x1 . (x0 x1)));
        |    g = (\x0 . x0);
        |    h = case x of {Nil -> Z | Cons x0 x1 -> (S Z)};
        |    p = let x0 = (g Z) in x0;
      """.stripMargin.trim()) {
      val code =
        """f g Z
          |  where
          |    f = \f x . f x;
          |    g = \x . x;
          |    h = case x of {Nil -> Z | Cons a b -> S Z};
          |    p = let x = g Z in x;
        """.stripMargin
      println(ProgPrinter(AstTransformer(Parser(code))))
      ProgPrinter(AstTransformer(Parser(code))).trim()
    }
  }
}
