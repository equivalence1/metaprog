package ru.mit.supercompilation.parser

import org.scalatest.FunSuite

class ParserTest extends FunSuite {
  test("Parser: lambda-expr success 1") {
    val expectedAst = LambdaNode("x" :: Nil, VarNode("x"))
    assertResult(expectedAst) {
      ProgParser("(\\x . x)")
    }
  }

  test("Parser: lambda-expr success 2") {
    val expectedAst = AppNode(
      LambdaNode("x" :: Nil, AppNode(
        LambdaNode("y" :: Nil, AppNode(
          VarNode("y"),
          VarNode("x"))),
        VarNode("x"))),
      LambdaNode("x" :: Nil, VarNode("x")))
    assertResult(expectedAst) {
      ProgParser("(\\x . (\\y . y x) x) (\\x . x)")
    }
  }

  test("Parser: application without braces success") {
    val expectedAst = AppNode(AppNode(VarNode("x"), VarNode("y")), VarNode("z"))
    assertResult(expectedAst) {
      ProgParser("x y z")
    }
  }

  test("Parser: lambda-expr fail 1") {
    assertThrows[RuntimeException] {
      ProgParser("(\\x . (\\y . y x x) (\\x . x)")
    }
  }

  test("Parser: lambda-expr fail 2") {
    assertThrows[RuntimeException] {
      ProgParser("(\\x . (\\y  y x) x) (\\x . x)")
    }
  }

  test("Parser: lambda-expr fail 3") {
    assertThrows[RuntimeException] {
      ProgParser("(\\x . (\\y . y . x) x) (\\x . x)")
    }
  }
}
