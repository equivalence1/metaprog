package ru.mit.supercompilation.parser

import org.scalatest.FunSuite

class ParserTest extends FunSuite {
  test("Parser: lambda-expr success 1") {
    val expectedAst = LambdaNode("x" :: Nil, IdentifierNode("x"))
    assertResult(expectedAst) {
      Parser("(\\x . x)")
    }
  }

  test("Parser: lambda-expr success 2") {
    val expectedAst = AppNode(
      LambdaNode("x" :: Nil, AppNode(
        LambdaNode("y" :: Nil, AppNode(
          IdentifierNode("y"),
          IdentifierNode("x"))),
        IdentifierNode("x"))),
      LambdaNode("x" :: Nil, IdentifierNode("x")))
    assertResult(expectedAst) {
      Parser("(\\x . (\\y . y x) x) (\\x . x)")
    }
  }

  test("Parser: application without braces success") {
    val expectedAst = AppNode(AppNode(IdentifierNode("x"), IdentifierNode("y")), IdentifierNode("z"))
    assertResult(expectedAst) {
      Parser("x y z")
    }
  }

  test("Parser: lambda-expr fail 1") {
    assertThrows[RuntimeException] {
      Parser("(\\x . (\\y . y x x) (\\x . x)")
    }
  }

  test("Parser: lambda-expr fail 2") {
    assertThrows[RuntimeException] {
      Parser("(\\x . (\\y  y x) x) (\\x . x)")
    }
  }

  test("Parser: lambda-expr fail 3") {
    assertThrows[RuntimeException] {
      Parser("(\\x . (\\y . y . x) x) (\\x . x)")
    }
  }
}
