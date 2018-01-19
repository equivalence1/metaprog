package ru.mit.supercompilation

import org.scalatest.FunSuite
import ru.mit.supercompilation.Types._

class InstanceTest extends FunSuite {
  test("simple instance test") {
    assertResult(true) {
      val expr1 = Case(GlobalVar("a"), List(CaseBranch("B", 0, BVar(1)), CaseBranch("A", 0, Constr("1", Nil))))
      val expr2 = Case(BVar(0), List(CaseBranch("B", 0, BVar(1)), CaseBranch("A", 0, Constr("1", Nil))))

      val instance = isInstance(expr1, expr2)

      instance match {
        case Some(_) => true
        case _ => false
      }
    }
  }
}