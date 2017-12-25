package ru.mit.supercompilation.parser

import org.scalatest.FunSuite
import ru.mit.supercompilation.Types._

class ProgramAstTransformerTest extends FunSuite {
  test("ExprBuilder: lambda-expr") {
    assertResult(App(Lambda(App(Lambda(App(Var(0), Var(1))), Var(0))), Lambda(Var(0)))) {
//      ProgramAstTransformer(AppNode(
//        LambdaNode("x" :: Nil, AppNode(
//          LambdaNode("y" :: Nil, AppNode(
//            IdentifierNode("y"),
//            IdentifierNode("x"))),
//          IdentifierNode("x"))),
//        LambdaNode("x" :: Nil, IdentifierNode("x"))))
      null
    }
  }
}
