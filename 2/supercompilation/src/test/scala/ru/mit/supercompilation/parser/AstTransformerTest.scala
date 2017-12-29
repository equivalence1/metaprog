package ru.mit.supercompilation.parser

import org.scalatest.FunSuite
import ru.mit.supercompilation.Types._

class AstTransformerTest extends FunSuite {
  test("program with definitions") {
    val expectedExpr = App(App(Fun("f"), Fun("g")), Constr("Z", Nil))
    val expectedFDef = ("f", Lambda(Lambda(App(BVar(1), BVar(0)))))
    val expectedGDef = ("g", Lambda(BVar(0)))
    val expectedHDef = ("h", Let(GlobalVar("global_var"), App(Fun("g"), BVar(0))))
    assertResult((expectedExpr, expectedFDef :: expectedGDef :: expectedHDef :: Nil)) {
      AstTransformer(Parser(
        """f g Z
          |  where
          |    f = \f x . f x;
          |    g = \x . x;
          |    h = let x = global_var in g x;
        """.stripMargin))
    }
  }
}
