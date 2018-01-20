package ru.mit.supercompilation.parser

import org.scalatest.FunSuite

class LexerTest extends FunSuite {
  test("Lexer: lambda-expr") {
    assertResult(OPEN_BRACKET :: LAMBDA :: IDENTIFIER("x") :: DOT :: OPEN_BRACKET :: LAMBDA :: IDENTIFIER("y") :: DOT ::
      IDENTIFIER("y") :: IDENTIFIER("x") :: CLOSE_BRACKET :: IDENTIFIER("x") :: CLOSE_BRACKET :: OPEN_BRACKET ::
      LAMBDA :: IDENTIFIER("x") :: DOT :: IDENTIFIER("x") :: CLOSE_BRACKET :: Nil) {
      Lexer("(\\x . (\\y . y x) x) (\\x . x)")
    }
  }

  test("Lexer: case-expr") {
    assertResult(CASE :: IDENTIFIER("x") :: OF :: CURLY_OPEN :: CONSTRUCTOR("Nil") :: ARROW ::
      IDENTIFIER("y") :: ALTERNATION :: CONSTRUCTOR("Cons") :: IDENTIFIER("a") :: IDENTIFIER("b") ::
      ARROW :: IDENTIFIER("z") :: CURLY_CLOSE :: Nil) {
      Lexer("case x of {Nil -> y | Cons a b -> z}")
    }
  }

  test("Lexer: full program") {
    assertResult(IDENTIFIER("f") :: CONSTRUCTOR("Z") ::
      WHERE :: IDENTIFIER("f") :: ASSIGNMENT :: LAMBDA :: IDENTIFIER("x") :: DOT :: IDENTIFIER("x") :: SEMICOLON :: Nil) {
      Lexer(
        """f Z
          | where
          |   f = \x . x;
        """.stripMargin)
    }
  }
}
