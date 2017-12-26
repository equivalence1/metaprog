package ru.mit.supercompilation.parser

import ru.mit.supercompilation.Types._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
  override def first: Token = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[Token] = new TokenReader(tokens.tail)
}

/**
  * Parses List[[Token]] produced by Lexer into [[ProgramAst]]
  * ProgramAst is almost what we need -- [[AstTransformer]] will do the rest
  * of the transformation into [[ru.mit.supercompilation.Types.Program]]
  */
object Parser extends Parsers {
  override type Elem = Token

  private def identifier: Parser[IdentifierNode] = {
    accept("identifier", { case IDENTIFIER(x) => IdentifierNode(x) })
  }

  private def emptyConstructor: Parser[ConstructorNode] = {
    accept("constructor", { case CONSTRUCTOR(x) => ConstructorNode(x, Nil) })
  }

  private def expr: Parser[ExprAst] = {
    val innerExpr = OPEN_BRACKET ~ expr ~ CLOSE_BRACKET ^^ {
      case _ ~ e ~ _ => e
    }
    val base = identifier | innerExpr | emptyConstructor

    val constructor = emptyConstructor ~ rep(base) ^^ {
      case ConstructorNode(name, _) ~ args => ConstructorNode(name, args)
    }

    val lambda = LAMBDA ~ rep1(identifier) ~ DOT ~ expr ^^ {
      case _ ~ vars ~ _ ~ e => LambdaNode(vars.map(ident => ident.name), e)
    }

    val app = base ~ rep1(base) ^^ {
      case e1 ~ es => es.foldLeft(e1) {(app, e) => AppNode(app, e)}
    }

    val let = LET ~ identifier ~ ASSIGNMENT ~ expr ~ IN ~ expr ^^ {
      case _ ~ ident ~ _ ~ e1 ~ _ ~ e2 => LetNode(ident.name, e1, e2)
    }

    // constructors in case should only have variables as arguments
    val caseConstructor = emptyConstructor ~ rep(identifier) ^^ {
      case ConstructorNode(name, _) ~ args => CaseConstructorNode(name, args.map(a => a.name))
    }
    val singleCase = caseConstructor ~ ARROW ~ expr ^^ {
      case (c@CaseConstructorNode(_, _)) ~ _ ~ e => (c, e)
    }
    val _case = CASE ~ expr ~ OF ~ CURLY_OPEN ~ repsep(singleCase, ALTERNATION) ~ CURLY_CLOSE ^^ {
      case _ ~ e ~ _ ~ _ ~ cases ~ _ => CaseNode(e, cases)
    }

    _case | let | lambda | app | constructor | base
  }

  private def functionDefinition: Parser[(String, ExprAst)] = {
    identifier ~ ASSIGNMENT ~ expr ~ SEMICOLON ^^ {
      case IdentifierNode(fName) ~ _ ~ e ~ _ => (fName, e)
    }
  }

  private def prog: Parser[ProgramAst] = {
    val onlyExpr = expr ^^ {
      e => (e, Nil)
    }
    val exprWithWhere = expr ~ WHERE ~ rep(functionDefinition) ^^ {
      case e ~ _ ~ fDefs => (e, fDefs)
    }

    phrase(exprWithWhere | onlyExpr)
  }

  def apply(code: String): ProgramAst = {
    val tokens = Lexer(code)
    val reader = new TokenReader(tokens)
    prog(reader) match {
      case NoSuccess(msg, _) => throw new RuntimeException(msg)
      case Success(result, _) => result
    }
  }
}

/**
  * Last stage of parsing.
  * Now we need to transform from [[ProgramAst]] into [[ru.mit.supercompilation.Types.Program]]
  * The main task is to transform identifiers into Fun, GlobalVar or Var.
  * Note that Expr uses de Bruijn indexes
  */
object AstTransformer {

  private var functionsList: List[String] = _

  private def exprTransform(ast: ExprAst, scopeVariablesStack: List[String]): Expr = {
    ast match {
      case IdentifierNode(name) =>
        if (scopeVariablesStack.contains(name)) {
          Var(scopeVariablesStack.indexOf(name))
        } else if (functionsList.contains(name)) {
          Fun(name)
        } else {
          GlobalVar(name)
        }

      case LambdaNode(vars, e) => vars match {
        case x :: xs => Lambda(exprTransform(LambdaNode(xs, e), x :: scopeVariablesStack))
        case Nil => exprTransform(e, scopeVariablesStack)
      }

      case AppNode(e1, e2) => App(exprTransform(e1, scopeVariablesStack), exprTransform(e2, scopeVariablesStack))

      case ConstructorNode(name, args) => Constr(name, args.map(arg => exprTransform(arg, scopeVariablesStack)))

      case LetNode(varName, e1, e2) =>
        Let(exprTransform(e1, scopeVariablesStack), exprTransform(e2, varName :: scopeVariablesStack))

      case CaseNode(selector, cases) =>
        val newCases =
          cases.map { a =>
            val c = a._1
            val e = a._2
            val newScope = c.args.reverse ++ scopeVariablesStack
            (c.name, c.args.size, exprTransform(e, newScope))
          }
        Case(exprTransform(selector, scopeVariablesStack), newCases)

      case CaseConstructorNode(_, _) => throw new RuntimeException("CaseConstructorNode outside case")
    }
  }

  def apply(ast: ProgramAst): Program = {
    functionsList = ast._2.map(definition => definition._1)
    (exprTransform(ast._1, Nil), ast._2.map(definition => (definition._1, exprTransform(definition._2, Nil))))
  }

}
