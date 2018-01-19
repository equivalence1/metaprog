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
  * ProgramAst is almost what we need -- [[ExprTranslator]] will do the rest
  * of the transformation into [[ru.mit.supercompilation.Types.Program]]
  */
object Parser extends Parsers {

  override type Elem = Token

  private def identifier: Parser[IdentifierNode] = {
    accept("identifier", { case IDENTIFIER(x) => IdentifierNode(x) })
  }

  private def emptyConstructor: Parser[ConstructorNode] = {
    accept("empty constructor", { case CONSTRUCTOR(x) => ConstructorNode(x, Nil) })
  }

  private def expr: Parser[ExprAst] = {
    val innerExpr = OPEN_BRACKET ~ expr ~ CLOSE_BRACKET ^^ {
      case _ ~ e ~ _ => e
    }
    val base = identifier | innerExpr | emptyConstructor

    val constructor = emptyConstructor ~ rep(base) ^^ {
      case ConstructorNode(name, Nil) ~ args => ConstructorNode(name, args)
    }

    val lambda = LAMBDA ~ rep1(identifier) ~ (DOT | ARROW) ~ expr ^^ {
      case _ ~ vars ~ _ ~ e => LambdaNode(vars.map(ident => ident.name), e)
    }

    val app = base ~ rep1(base) ^^ {
      case e1 ~ es => es.foldLeft(e1) {(app, e) => AppNode(app, e)}
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

    _case | lambda | constructor | app | base
  }

  private def functionDefinition: Parser[AstFDef] = {
    identifier ~ ASSIGNMENT ~ expr ~ SEMICOLON ^^ {
      case IdentifierNode(fName) ~ _ ~ e ~ _ => AstFDef(fName, e)
    }
  }

  private def prog: Parser[ProgramAst] = {
    val onlyExpr = expr ^^ {
      e => ProgramAst(e, Nil)
    }
    val exprWithWhere = expr ~ WHERE ~ rep(functionDefinition) ^^ {
      case e ~ _ ~ fDefs => ProgramAst(e, fDefs)
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
object ExprTranslator {

  private[this] var functionsList: List[String] = _

  private[this] def translateToExpr(ast: ExprAst, scopeVariablesStack: List[String]): Expr = {
    ast match {
      case IdentifierNode(name) =>
        if (scopeVariablesStack.contains(name)) {
          BVar(scopeVariablesStack.indexOf(name))
        } else if (functionsList.contains(name)) {
          Fun(name)
        } else {
          GlobalVar(name)
        }

      case LambdaNode(vars, e) => vars match {
        case x :: xs => Lambda(translateToExpr(LambdaNode(xs, e), x :: scopeVariablesStack))
        case Nil => translateToExpr(e, scopeVariablesStack)
      }

      case AppNode(e1, e2) => App(translateToExpr(e1, scopeVariablesStack), translateToExpr(e2, scopeVariablesStack))

      case ConstructorNode(name, args) => Constr(name, args.map(arg => translateToExpr(arg, scopeVariablesStack)))

      case CaseNode(selector, cases) =>
        val newCases =
          cases.map { branch =>
            val constr = branch._1
            val e = branch._2
            val newScope = constr.args.reverse ++ scopeVariablesStack
            CaseBranch(constr.name, constr.args.size, translateToExpr(e, newScope))
          }
        Case(translateToExpr(selector, scopeVariablesStack), newCases)

      case CaseConstructorNode(_, _) => throw new RuntimeException("CaseConstructorNode outside case")
    }
  }

  def apply(ast: ProgramAst): Program = {
    functionsList = ast.fDefs.map(definition => definition.fName)
    val translatedFdefs = ast.fDefs.map(definition => FDef(definition.fName, translateToExpr(definition.body, Nil)))
    Program(translateToExpr(ast.mainExpr, Nil), translatedFdefs)
  }

}
