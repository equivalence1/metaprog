package ru.mit.supercompilation

import org.scalatest.FunSuite
import ru.mit.supercompilation.Types._

class ReducerTest extends FunSuite {

  // normalization tests

  test("normalization lambda obs") {
    val sExpr = """\x . x x"""
    assertResult((Lambda(App(BVar(0), BVar(0))), Nil)) {
      normalize(parseExpr(sExpr))
    }
  }

  test("normalization lambda redex") {
    val sExpr = """(\x . x) x"""
    assertResult((App(Lambda(BVar(0)), GlobalVar("x")), Nil)) {
      normalize(parseExpr(sExpr))
    }
  }

  test("normalization app") {
    val sExpr = """x a"""
    assertResult((App(GlobalVar("x"), GlobalVar("a")), Nil)) {
      normalize(parseExpr(sExpr))
    }
  }

//  test("normalization case") {
//    val sExpr = """case f of {Nil -> Nil}"""
//    assertResult((Case(GlobalVar("f"), List(("Nil", 0, Constr("Nil", Nil)))), Nil)) {
//      normalize(parseExpr(sExpr))
//    }
//  }

  // reduce tests

//  test("reduceStep app") {
//    val sExpr = """(\x . x) (\y . y)"""
//    assertResult("""(\x0 . x0)""") {
//      exprToString(Reducer.nReduceStep(normalize(parseExpr(sExpr)), Nil)._2._1)
//    }
//  }
//
//  test("reduceStep lambda") {
//    val sExpr = """(\x . \y . \z . f) (\y . y)"""
//    assertResult("""(\x0 . (\x1 . f))""") {
//      exprToString(Reducer.nReduceStep(normalize(parseExpr(sExpr)), Nil)._2._1)
//    }
//  }
//
//  test("reduceStep case") {
//    val sExpr = """case Nil of {NotNil -> Nil | Nil -> case f of {Nil -> Nil}}"""
//    val reducedExpr = Reducer.nReduceStep(normalize(parseExpr(sExpr)), Nil)._2
//    assertResult("""case f of {Nil -> Nil}""") {
//      exprToString(reducedExpr._1)
//    }
//    assertResult(Nil) {
//      reducedExpr._2
//    }
//  }
//
//  test("reduce complex") {
//    val sProg =
//      """foo (case Nil of {NotNil -> Nil | Nil -> case (\x . x) NotNil of {NotNil -> Nil}})
//        |  where
//        |    foo = \x . case x of {NotNil -> Fail | Nil -> Success};
//      """.stripMargin
//    val prog = parseProg(sProg)
//    val reducedExpr = Reducer.nReduce(normalize(prog._1), prog._2)._2
//    assertResult("""Success""") {
//      exprToString(reducedExpr._1)
//    }
//  }

}