package ru.mit.supercompilation.parser

import org.scalatest.FunSuite
import ru.mit.supercompilation.Types._

class AstTransformerTest extends FunSuite {
  test("program with definitions") {
    val expectedExpr = App(App(Fun("f"), Fun("g")), Constr("Z", Nil))
    val expectedFDef = ("f", Lambda(Lambda(App(BVar(1), BVar(0)))))
    val expectedGDef = ("g", Lambda(BVar(0)))
    assertResult((expectedExpr, List(expectedFDef, expectedGDef))) {
      ExprTranslator(Parser(
        """f g Z
          |  where
          |    f = \f x . f x;
          |    g = \x . x;
        """.stripMargin))
    }
  }
}
