package ru.mit.supercompilation.parser

import org.scalatest.FunSuite

class ParserTest extends FunSuite {
  test("lambda-expr success 1") {
    val expectedAst = LambdaNode("x" :: Nil, IdentifierNode("x"))
    assertResult(expectedAst) {
      Parser("(\\x . x)").mainExpr
    }
  }

  test("lambda-expr success 2") {
    val expectedAst = AppNode(
      LambdaNode("x" :: Nil, AppNode(
        LambdaNode("y" :: Nil, AppNode(
          IdentifierNode("y"),
          IdentifierNode("x"))),
        IdentifierNode("x"))),
      LambdaNode("x" :: Nil, IdentifierNode("x")))
    assertResult(expectedAst) {
      Parser("(\\x . (\\y . y x) x) (\\x . x)").mainExpr
    }
  }

  test("application without braces success") {
    val expectedAst = AppNode(AppNode(IdentifierNode("x"), IdentifierNode("y")), IdentifierNode("z"))
    assertResult(expectedAst) {
      Parser("x y z").mainExpr
    }
  }

  test("lambda-expr fail 1") {
    assertThrows[RuntimeException] {
      Parser("(\\x . (\\y . y x x) (\\x . x)")
    }
  }

  test("lambda-expr fail 2") {
    assertThrows[RuntimeException] {
      Parser("(\\x . (\\y  y x) x) (\\x . x)")
    }
  }

  test("lambda-expr fail 3") {
    assertThrows[RuntimeException] {
      Parser("(\\x . (\\y . y . x) x) (\\x . x)")
    }
  }

  test("constructor success") {
    val expected = ConstructorNode("C", List(IdentifierNode("a"), IdentifierNode("b")))
    assertResult(expected) {
      Parser("C a b").mainExpr
    }
  }

  test("case-expr success") {
    val expected = CaseNode(IdentifierNode("x"), (CaseConstructorNode("Nil", Nil), IdentifierNode("z")) ::
      (CaseConstructorNode("Cons", "a" :: "b" :: Nil), IdentifierNode("z")) :: Nil)
    assertResult(expected) {
      Parser("case x of {Nil -> z | Cons a b -> z}").mainExpr
    }
  }

  test("program with definitions") {
    val expectedExpr = AppNode(AppNode(IdentifierNode("f"), IdentifierNode("g")), ConstructorNode("Z", Nil))
    val expectedFDef = ("f", LambdaNode("f" :: "x" :: Nil, AppNode(IdentifierNode("f"), IdentifierNode("x"))))
    val expectedGDef = ("g", LambdaNode("x" :: Nil, IdentifierNode("x")))
    assertResult((expectedExpr, expectedFDef :: expectedGDef :: Nil)) {
      Parser(
        """f g Z
          |  where
          |    f = \f x . f x;
          |    g = \x . x;
        """.stripMargin)
    }
  }
}
