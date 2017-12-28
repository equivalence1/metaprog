package ru.mit.supercompilation

import org.scalatest.FunSuite
import ru.mit.supercompilation.Types._

class GeneralizationTest extends FunSuite {
  test("lambda expr") {
    val e1 = parseExpr("""\a . \b . b glob""")
    val e2 = parseExpr("""\c . \d . d glob""")
    val generalization = Generalization(e1, e2)

    assertResult(Lambda(Lambda(App(Var(0), GlobalVar("glob"))))) {
      generalization._1
    }
    assertResult("""(\x0 . (\x1 . (x1 glob)))""") {
      exprToString(subst(generalization._1, generalization._2))
    }
    assertResult("""(\x0 . (\x1 . (x1 glob)))""") {
      exprToString(subst(generalization._1, generalization._3))
    }

    val e3 = parseExpr("""\a . \b . b a""")
    val e4 = parseExpr("""\c . \d . c d""")
    val generalization2 = Generalization(e3, e4)

    assertResult(Lambda(Lambda(App(ConfVar(0), ConfVar(1))))) {
      generalization2._1
    }
    assertResult("""(\x0 . (\x1 . (x1 x0)))""") {
      exprToString(subst(generalization2._1, generalization2._2))
    }
    assertResult("""(\x0 . (\x1 . (x0 x1)))""") {
      exprToString(subst(generalization2._1, generalization2._3))
    }
  }
}
