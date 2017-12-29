package ru.mit.supercompilation.printer

import org.scalatest.FunSuite
import ru.mit.supercompilation.Types.{App, BVar, Lambda}
import ru.mit.supercompilation.parser.{AstTransformer, Parser}

class ExprPrinterTest extends FunSuite {
  test("lambda-expr") {
    assertResult("((\\x0 . ((\\x1 . (x1 x0)) x0)) (\\x0 . x0))") {
      ProgPrinter(App(Lambda(App(Lambda(App(BVar(0), BVar(1))), BVar(0))), Lambda(BVar(0))))
    }
  }

  test("full program") {
    assertResult(
      """((f g) Z)
        |  where
        |    f = (\x0 . (\x1 . (x0 x1)));
        |    g = (\x0 . x0);
        |    h = case x of {Nil -> Z | Cons x0 x1 -> S Z};
        |    p = let x0 = (g Z) in let x1 = x0 in x1;
      """.stripMargin.trim()) {
      val code =
        """f g Z
          |  where
          |    f = \f x . f x;
          |    g = \x . x;
          |    h = case x of {Nil -> Z | Cons a b -> S Z};
          |    p = let x = g Z in let y = x in y;
        """.stripMargin
      ProgPrinter(AstTransformer(Parser(code))).trim()
    }
  }
}
