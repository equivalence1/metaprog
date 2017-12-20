package ru.mit.supercompilation.parser

import org.scalatest.FunSuite

class LexerTest extends FunSuite {
  test("Lexer: lambda-expr") {
    assertResult(Right(OPEN_BRACKET :: LAMBDA :: IDENTIFIER("x") :: DOT :: OPEN_BRACKET :: LAMBDA :: IDENTIFIER("y") :: DOT ::
      IDENTIFIER("y") :: IDENTIFIER("x") :: CLOSE_BRACKET :: IDENTIFIER("x") :: CLOSE_BRACKET :: OPEN_BRACKET ::
      LAMBDA :: IDENTIFIER("x") :: DOT :: IDENTIFIER("x") :: CLOSE_BRACKET :: Nil)) {
      ProgLexer("(\\x . (\\y . y x) x) (\\x . x)")
    }
  }
}
