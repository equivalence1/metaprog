package ru.mit.supercompilation

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    if (args.nonEmpty) {
      // I don't see any point in handling exceptions in this case
      println(Supercompiler(Source.fromFile(args(0)).getLines.mkString("\n")))
    }
  }

}
