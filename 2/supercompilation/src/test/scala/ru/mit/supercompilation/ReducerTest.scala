package ru.mit.supercompilation

import org.scalatest.FunSuite
import ru.mit.supercompilation.Types._

class ReducerTest extends FunSuite {

  // normalization tests

  test("normalization lambda obs") {
    val sExpr = """\x . x x"""
    assertResult(Lambda(App(BVar(0), BVar(0)))) {
      normalize(parseExpr(sExpr)).left.get
    }
  }

  test("normalization lambda redex") {
    val sExpr = """(\x . x) x"""
    assertResult((App(Lambda(BVar(0)), GlobalVar("x")), Nil)) {
      normalize(parseExpr(sExpr)).right.get
    }
  }

  test("normalization app") {
    val sExpr = """x a b"""
    assertResult(App(App(GlobalVar("x"), GlobalVar("a")), GlobalVar("b"))) {
      normalize(parseExpr(sExpr)).left.get
    }
  }

  test("normalization case") {
    val sExpr = """case f of {Nil -> Nil}"""
    assertResult((Case(GlobalVar("f"), List(CaseBranch("Nil", 0, Constr("Nil", Nil)))), Nil)) {
      normalize(parseExpr(sExpr)).right.get
    }
  }

  // reduce tests

  test("reduceStep app") {
    val sExpr = """(\x . x) (\y . y)"""
    assertResult("""(\x0 -> x0)""") {
      exprToString(Reducer.nReduceStep(normalize(parseExpr(sExpr)), Nil).left.get)
    }
  }

  test("reduceStep lambda") {
    val sExpr = """(\x . \y . \z . f) (\y . y)"""
    assertResult("""(\x0 -> (\x1 -> f))""") {
      exprToString(Reducer.nReduceStep(normalize(parseExpr(sExpr)), Nil).left.get)
    }
  }

  test("reduceStep case") {
    val sExpr = """case Nil of {NotNil -> Nil | Nil -> case f of {Nil -> Nil}}"""
    val reducedExpr = Reducer.nReduceStep(normalize(parseExpr(sExpr)), Nil).right.get
    assertResult("""case f of {Nil -> Nil}""") {
      exprToString(reducedExpr._1)
    }
    assertResult(Nil) {
      reducedExpr._2
    }
  }

  test("reduce complex") {
    val sProg =
      """foo (case Nil of {NotNil -> Nil | Nil -> case (\x . x) NotNil of {NotNil -> Nil}})
        |  where
        |    foo = \x . case x of {Nil -> Success | NotNil -> Fail};
      """.stripMargin
    val prog = parseProg(sProg)
    val reducedExpr = Reducer.nReduce(normalize(prog.mainExpr), prog.fdefs).left.get
    assertResult("""Success""") {
      exprToString(reducedExpr)
    }
  }

}