package ru.mit.supercompilation.parser

import org.scalatest.FunSuite
import ru.mit.supercompilation.Types._

class ExprBuilderTest extends FunSuite {
  test("ExprBuilder: lambda-expr") {
    assertResult(App(Lambda(App(Lambda(App(Var(0), Var(1))), Var(0))), Lambda(Var(0)))) {
      ExprBuilder(AppNode(
        LambdaNode("x" :: Nil, AppNode(
          LambdaNode("y" :: Nil, AppNode(
            VarNode("y"),
            VarNode("x"))),
          VarNode("x"))),
        LambdaNode("x" :: Nil, VarNode("x"))))
    }
  }
}
