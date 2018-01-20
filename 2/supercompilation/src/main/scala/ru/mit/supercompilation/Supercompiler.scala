package ru.mit.supercompilation

import ru.mit.supercompilation.printer.ProgPrinter

object Supercompiler {

  private[this] var nextId: Int = _
  private[this] var nextFunId: Int = _

  def nextFreeIndex(): Int = {
    nextId += 1
    nextId
  }

  def nextFreeFunIndex(): Int = {
    nextFunId += 1
    nextFunId
  }

  def apply(code: String): String = {
    nextFunId = -1
    nextId = -1

    val prog = parseProg(code)
    val ppt = new Ppt(prog)
    ppt.build()
    ProgPrinter(ppt.residualize())
  }

}
