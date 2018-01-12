package ru.mit.supercompilation

import org.scalatest.FunSuite

class PptTest extends FunSuite {
  test("reduce complex") {
    val sProg =
      """foo (case Nil of {NotNil -> Nil | Nil -> case (\x . x) NotNil of {NotNil -> Nil}})
        |  where
        |    foo = \x . case x of {NotNil -> Fail | Nil -> Success};
      """.stripMargin
    val prog = parseProg(sProg)
    val ppt = new Ppt(prog)
    ppt.build()
    println(ppt.root)
    println(ppt.residualize()._1)
  }
}