package ru.mit.supercompilation.parser

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

sealed trait ExprAst
// It's impossible to distinguish variables from functions at this point,
// only ExprBuilder can do this.
// Thus IdentifierNode, not VarNode and FunNode
case class IdentifierNode(s: String) extends ExprAst
case class LambdaNode(varNames: List[String], e: ExprAst) extends ExprAst
case class AppNode(e1: ExprAst, e2: ExprAst) extends ExprAst

class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
  override def first: Token = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[Token] = new TokenReader(tokens.tail)
}

/**
  * Parses List[Token] produced by ProgLexer into ExprAst
  */
object TokensParser extends Parsers {
  override type Elem = Token

  private def identifier: Parser[IdentifierNode] = {
    accept("identifier", { case IDENTIFIER(x) => IdentifierNode(x) })
  }

  def program: Parser[ExprAst] = {
    phrase(expr) // TODO phrase -- because no function definitions for now
  }

  def expr: Parser[ExprAst] = {
    val innerExpr = OPEN_BRACKET ~ expr ~ CLOSE_BRACKET ^^ {
      case _ ~ e ~ _ => e
    }
    val base = identifier | innerExpr
    val lambda = LAMBDA ~ rep1(identifier) ~ DOT ~ expr ^^ {
      case _ ~ vars ~ _ ~ e => LambdaNode(vars.map(ident => ident.s), e)
    }
    val app = base ~ rep1(base) ^^ {
      case e1 ~ es => es.foldLeft(e1) {(app: ExprAst, e: ExprAst) => AppNode(app, e)}
    }
    app | lambda | base // Order is important. app must go before variable and innerExpr
  }

  def apply(tokens: Seq[Token]): ExprAst = {
    val reader = new TokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, _) => throw new RuntimeException(msg)
      case Success(result, _) => result
    }
  }
}

object ProgParser {
  def apply(code: String): ExprAst = {
    TokensParser(ProgLexer(code))
  }
}
