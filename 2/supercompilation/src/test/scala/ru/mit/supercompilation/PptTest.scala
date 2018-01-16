package ru.mit.supercompilation

import org.scalatest.FunSuite
import ru.mit.supercompilation.printer.ProgPrinter

class PptTest extends FunSuite {
  test("Success") {
    val sProg =
      """foo (case Nil of {NotNil -> Nil | Nil -> case (\x . x) NotNil of {NotNil -> Nil}})
        |  where
        |    foo = \x . case x of {NotNil -> Fail | Nil -> Success};
      """.stripMargin
    val prog = parseProg(sProg)
    val ppt = new Ppt(prog)
    //ppt.build()
    //println(ppt.root)
    //println(ProgPrinter(ppt.residualize()))
  }

  test("snd") {
    val sProg =
      """snd (Cons 1 (snd (Cons 2 3)))
        |    where
        |      snd = \x . case x of { Cons a b -> b };
      """.stripMargin
    val prog = parseProg(sProg)
    val ppt = new Ppt(prog)
    //ppt.build()
    //println(ppt.root)
    //println(ProgPrinter(ppt.residualize()))
  }

    test("bla-bla-bla") {
    val sProg =
      """foo 1 xs
        |    where
        |      foo = \x . \xs . case xs of {Cons a b -> foo x b
        |                                    | Nil -> x};
      """.stripMargin
    val prog = parseProg(sProg)
    val ppt = new Ppt(prog)
//    ppt.build()
//    println(ppt.root)
//    println(ppt.residualize())
//    println(ProgPrinter(ppt.residualize()))
  }

  test("Example 2") {
    val sProg =
      """id (apply id (app xs ys))
        |  where
        |    id = \x -> x;
        |    apply = \f -> \x -> f x;
        |    app = \xs -> \ys -> case xs of { Nil -> ys
        |                                   | Cons x xs -> Cons x (app xs ys) };
      """.stripMargin
    val prog = parseProg(sProg)
    val ppt = new Ppt(prog)
//    ppt.build()
//    println(ppt.root)
//    println(ppt.residualize())
//    println(ProgPrinter(ppt.residualize()))
  }

  test("Example 3") {
    val sProg =
      """app (app xs ys) zs
        |    where
        |      app = \xs -> \ys -> case xs of { Nil -> ys | Cons x xs -> Cons x (app xs ys) };
      """.stripMargin
    val prog = parseProg(sProg)
    val ppt = new Ppt(prog)
    ppt.build()
    println(ppt.root)
    println(ppt.residualize())
    println(ProgPrinter(ppt.residualize()))
  }

  test("Example 4") {
    val sProg =
      """sum (map (\x -> mult x x) (int Z n))
        |  where
        |    int = \l -> \r -> case gt l r of { True -> Nil
        |                                     | False -> Cons l (int (S l) r) };
        |    map = \f -> \xs -> case xs of { Nil -> Nil
        |                                  | Cons x xs -> Cons (f x) (map f xs) };
        |    sum = \xs -> case xs of { Nil -> Z
        |                            | Cons x xs -> plus x (sum xs) };
      """.stripMargin
    val prog = parseProg(sProg)
    val ppt = new Ppt(prog)
    ppt.build()
    println(ppt.root)
    println(ppt.residualize())
    println(ProgPrinter(ppt.residualize()))
  }
}
