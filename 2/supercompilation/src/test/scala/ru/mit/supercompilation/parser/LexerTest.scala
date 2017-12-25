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

  test("Lexer: let-expr") {
    assertResult(LET :: IDENTIFIER("x") :: ASSIGNMENT :: IDENTIFIER("Nil") :: IN :: IDENTIFIER("x") :: Nil) {
      Lexer("let x = Nil in x")
    }
  }

  test("Lexer: case-expr") {
    assertResult(CASE :: IDENTIFIER("x") :: OF :: CURLY_OPEN :: IDENTIFIER("Nil") :: ARROW ::
      IDENTIFIER("y") :: ALTERNATION :: IDENTIFIER("Cons") :: IDENTIFIER("a") :: IDENTIFIER("b") ::
      ARROW :: IDENTIFIER("z") :: CURLY_CLOSE :: Nil) {
      Lexer("case x of {Nil -> y | Cons a b -> z}")
    }
  }

  test("Lexer: full program") {
    assertResult(LET :: IDENTIFIER("x") :: ASSIGNMENT :: IDENTIFIER("f") :: IDENTIFIER("Z") :: IN :: IDENTIFIER("x") ::
      WHERE :: IDENTIFIER("f") :: ASSIGNMENT :: LAMBDA :: IDENTIFIER("x") :: DOT :: IDENTIFIER("x") :: Nil) {
      Lexer(
        """let x = f Z in x
          | where
          |   f = \x . x
        """.stripMargin)
    }
  }
}
