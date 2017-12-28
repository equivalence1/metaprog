package ru.mit.supercompilation

import org.scalatest.FunSuite
import ru.mit.supercompilation.Types._

class GeneralizationTest extends FunSuite {
  test("lambda expr") {
    nextId = -1

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

  test("Constr expr") {
    nextId = -1

    val e1 = parseExpr("""Constr (\a . a b) (b)""")
    val e2 = parseExpr("""Constr (\b . b b) (b)""")
    val sRes1 = """Constr (\x0 . (x0 b)) b"""
    val sRes2 = """Constr (\x0 . (x0 x0)) b"""
    val eRes = Constr("Constr", List(Lambda(App(Var(0), ConfVar(0))), GlobalVar("b")))

    val generalization = Generalization(e1, e2)

    assertResult(eRes) {
      generalization._1
    }
    assertResult(sRes1) {
      exprToString(subst(generalization._1, generalization._2))
    }
    assertResult(sRes2) {
      exprToString(subst(generalization._1, generalization._3))
    }
  }

  test("Let expr") {
    nextId = -1

    val e1 = parseExpr("""let x = Nil in let y = Nil in x""")
    val e2 = parseExpr("""let x = Nil in let y = Nil in y""")
    val sRes1 = """let x0 = Nil in let x1 = Nil in x0"""
    val sRes2 = """let x0 = Nil in let x1 = Nil in x1"""
    val eRes = Let(Constr("Nil", Nil), Let(Constr("Nil", Nil), ConfVar(0)))

    val generalization = Generalization(e1, e2)

    assertResult(eRes) {
      generalization._1
    }
    assertResult(sRes1) {
      exprToString(subst(generalization._1, generalization._2))
    }
    assertResult(sRes2) {
      exprToString(subst(generalization._1, generalization._3))
    }
  }

  test("Case expr 1") {
    nextId = -1

    val e1 = parseExpr("""case x of {Nil -> x | Cons a b -> a}""")
    val e2 = parseExpr("""case x of {Nil -> x | Cons a b -> b}""")
    val sRes1 = """case x of {Cons x0 x1 -> x0 | Nil -> x}"""
    val sRes2 = """case x of {Cons x0 x1 -> x1 | Nil -> x}"""
    val eRes = Case(GlobalVar("x"), List(("Cons", 2, ConfVar(0)), ("Nil", 0, GlobalVar("x"))))

    val generalization = Generalization(e1, e2)

    assertResult(eRes) {
      generalization._1
    }
    assertResult(sRes1) {
      val tmp = subst(generalization._1, generalization._2)
      exprToString(tmp)
    }
    assertResult(sRes2) {
      exprToString(subst(generalization._1, generalization._3))
    }
  }

  test("Case expr 2") {
    nextId = -1

    val e1 = parseExpr("""case x of {Nil -> x | Constr a b -> a}""")
    val e2 = parseExpr("""case x of {Nil -> x | Cons a b -> b}""")
    val sRes1 = """case x of {Nil -> x | Constr x0 x1 -> x0}"""
    val sRes2 = """case x of {Nil -> x | Cons x0 x1 -> x1}"""
    val eRes = ConfVar(0)

    val generalization = Generalization(e1, e2)

    assertResult(eRes) {
      generalization._1
    }
    assertResult(sRes1) {
      exprToString(subst(generalization._1, generalization._2))
    }
    assertResult(sRes2) {
      exprToString(subst(generalization._1, generalization._3))
    }
  }
}
