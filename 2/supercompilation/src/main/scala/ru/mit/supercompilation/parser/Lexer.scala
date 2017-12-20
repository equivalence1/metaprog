package ru.mit.supercompilation.parser

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

sealed trait Token
case class IDENTIFIER(s: String) extends Token
case object LAMBDA extends Token
case object DOT extends Token
case object OPEN_BRACKET extends Token
case object CLOSE_BRACKET extends Token

/**
  * We want to be able to parse regular string input (i.e. file with program) so we don't
  * have to build Expr manually. Thus Lexer + Parser.
  *
  * This lexer splits input string into List[Token]
  */
object ProgLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace: Regex = "[ \t\r\f]+".r

  def identifier:    Parser[IDENTIFIER]         = "[a-zA-Z_][a-zA-Z0-9_]*".r  ^^   {str => IDENTIFIER(str)}
  def lambda:        Parser[LAMBDA.type]        = "\\"                        ^^   {_ => LAMBDA}
  def dot:           Parser[DOT.type]           = "."                         ^^   {_ => DOT}
  def open_bracket:  Parser[OPEN_BRACKET.type]  = "("                         ^^   {_ => OPEN_BRACKET}
  def close_bracket: Parser[CLOSE_BRACKET.type] = ")"                         ^^   {_ => CLOSE_BRACKET}

  def tokenize: Parser[List[Token]] = {
    phrase(rep1(identifier | lambda | dot | open_bracket | close_bracket))
  }

  def apply(code: String): List[Token] = {
    parse(tokenize, code) match {
      case NoSuccess(msg, _) => throw new RuntimeException(msg)
      case Success(result, _) => result
    }
  }
}


