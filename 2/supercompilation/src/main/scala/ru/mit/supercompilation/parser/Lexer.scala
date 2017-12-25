package ru.mit.supercompilation.parser

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
  * We want to be able to parse regular string input (i.e. file with program) so we don't
  * have to build Expr manually. Thus Lexer + Parser.
  *
  * This lexer splits input string into List[Token]
  */
object Lexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace: Regex = "[ \t\r\f\n]+".r

  def identifier:    Parser[IDENTIFIER]         = "[a-zA-Z_][a-zA-Z0-9_]*".r  ^^   {str => IDENTIFIER(str)}
  def lambda:        Parser[LAMBDA.type]        = "\\"                        ^^   {_ => LAMBDA}
  def dot:           Parser[DOT.type]           = "."                         ^^   {_ => DOT}
  def openBracket:   Parser[OPEN_BRACKET.type]  = "("                         ^^   {_ => OPEN_BRACKET}
  def closeBracket:  Parser[CLOSE_BRACKET.type] = ")"                         ^^   {_ => CLOSE_BRACKET}
  def curlyOpen:     Parser[CURLY_OPEN.type]    = "{"                         ^^   {_ => CURLY_OPEN}
  def curlyClose:    Parser[CURLY_CLOSE.type]   = "}"                         ^^   {_ => CURLY_CLOSE}
  def let:           Parser[LET.type]           = "let"                       ^^   {_ => LET}
  def in:            Parser[IN.type]            = "in"                        ^^   {_ => IN}
  def _case:         Parser[CASE.type]          = "case"                      ^^   {_ => CASE}
  def of:            Parser[OF.type]            = "of"                        ^^   {_ => OF}
  def arrow:         Parser[ARROW.type]         = "->"                        ^^   {_ => ARROW}
  def alternation:   Parser[ALTERNATION.type]   = "|"                         ^^   {_ => ALTERNATION}
  def assignment:    Parser[ASSIGNMENT.type]    = "="                         ^^   {_ => ASSIGNMENT}
  def where:         Parser[WHERE.type]         = "where"                     ^^   {_ => WHERE}

  private def tokenize: Parser[List[Token]] = {
    phrase(rep1(
        lambda
        | dot
        | openBracket
        | closeBracket
        | curlyOpen
        | curlyClose
        | let
        | in
        | _case
        | of
        | arrow
        | alternation
        | assignment
        | where
        | identifier
    ))
  }

  def apply(code: String): List[Token] = {
    parse(tokenize, code) match {
      case NoSuccess(msg, _) => throw new RuntimeException(msg)
      case Success(result, _) => result
    }
  }
}


