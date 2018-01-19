package ru.mit.supercompilation

import org.scalatest.FunSuite
import ru.mit.supercompilation.Types._

class GeneralizationTest extends FunSuite {
  test("lambda expr") {
    val e1 = parseExpr("""\a . \b . b glob""")
    val e2 = parseExpr("""\c . \d . d glob""")
    val generalization = generalize(e1, e2)

    assertResult(Lambda(Lambda(App(BVar(0), GlobalVar("glob"))))) {
      generalization.gExpr
    }
    assertResult("""(\x0 -> (\x1 -> (x1 glob)))""") {
      exprToString(subst(generalization.gExpr, generalization.subst1))
    }
    assertResult("""(\x0 -> (\x1 -> (x1 glob)))""") {
      exprToString(subst(generalization.gExpr, generalization.subst2))
    }

    val e3 = parseExpr("""\a . \b . b a""")
    val e4 = parseExpr("""\c . \d . c d""")
    val generalization2 = generalize(e3, e4)

    assertResult("""(\x0 -> (\x1 -> (x1 x0)))""") {
      exprToString(subst(generalization2.gExpr, generalization2.subst1))
    }
    assertResult("""(\x0 -> (\x1 -> (x0 x1)))""") {
      exprToString(subst(generalization2.gExpr, generalization2.subst2))
    }
  }

  test("Constr expr") {
    val e1 = parseExpr("""Constr (\a . a b) (b)""")
    val e2 = parseExpr("""Constr (\b . b b) (b)""")
    val sRes1 = """(Constr (\x0 -> (x0 b)) b)"""
    val sRes2 = """(Constr (\x0 -> (x0 x0)) b)"""

    val generalization = generalize(e1, e2)

    assertResult(sRes1) {
      exprToString(subst(generalization.gExpr, generalization.subst1))
    }
    assertResult(sRes2) {
      exprToString(subst(generalization.gExpr, generalization.subst2))
    }
  }

  test("Case expr 1") {
    val e1 = parseExpr("""case x of {Nil -> x | Cons a b -> a}""")
    val e2 = parseExpr("""case x of {Nil -> x | Cons a b -> b}""")
    val sRes1 = """case x of {Cons x0 x1 -> x0 | Nil -> x}"""
    val sRes2 = """case x of {Cons x0 x1 -> x1 | Nil -> x}"""

    val generalization = generalize(e1, e2)

    assertResult(sRes1) {
      exprToString(subst(generalization.gExpr, generalization.subst1))
    }
    assertResult(sRes2) {
      exprToString(subst(generalization.gExpr, generalization.subst2))
    }
  }

  test("Case expr 2") {
    val e1 = parseExpr("""case x of {Nil -> x | Constr a b -> a}""")
    val e2 = parseExpr("""case x of {Nil -> x | Cons a b -> b}""")
    val sRes1 = """case x of {Nil -> x | Constr x0 x1 -> x0}"""
    val sRes2 = """case x of {Nil -> x | Cons x0 x1 -> x1}"""

    val generalization = generalize(e1, e2)

    assertResult(sRes1) {
      exprToString(subst(generalization.gExpr, generalization.subst1))
    }
    assertResult(sRes2) {
      exprToString(subst(generalization.gExpr, generalization.subst2))
    }
  }
}
