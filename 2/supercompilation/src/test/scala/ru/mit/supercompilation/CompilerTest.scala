package ru.mit.supercompilation

import org.scalatest.{BeforeAndAfterEach, FunSuite}

/**
  * It's actually pretty hard to even define in which situation
  * a test is `passed`. So for now just print results on some test
  * examples
  */
class CompilerTest extends FunSuite with BeforeAndAfterEach {

  var curTest = 0

  override def beforeEach(): Unit = {
    curTest += 1
    println(s"     Example $curTest:\n")
  }

  override def afterEach(): Unit = {
    println("\n====================\n")
  }

  test("Example 1") {
    val code =
      """foo (case Nil of {NotNil -> Nil | Nil -> case (\x . x) NotNil of {NotNil -> Nil}})
        |  where
        |    foo = \x . case x of {NotNil -> Fail | Nil -> Success};
      """.stripMargin
    println("before:")
    println(code)
    println("\nafter:")
    println(Supercompiler(code))
  }

  test("Example 2") {
    val code =
      """snd (Cons 1 (snd (Cons 2 3)))
        |    where
        |      snd = \x . case x of { Cons a b -> b };
      """.stripMargin
    println("before:")
    println(code)
    println("\nafter:")
    println(Supercompiler(code))
  }

    test("Example 3") {
    val code =
      """foo 1 xs
        |    where
        |      foo = \x . \xs . case xs of {Cons a b -> foo x b
        |                                    | Nil -> x};
      """.stripMargin
      println("before:")
      println(code)
      println("\nafter:")
      println(Supercompiler(code))
  }

  test("Example 4") {
    val code =
      """id (apply id (app xs ys))
        |  where
        |    id = \x -> x;
        |    apply = \f -> \x -> f x;
        |    app = \xs -> \ys -> case xs of { Nil -> ys
        |                                   | Cons x xs -> Cons x (app xs ys) };
      """.stripMargin
    println("before:")
    println(code)
    println("\nafter:")
    println(Supercompiler(code))
  }

  test("Example 5") {
    val code =
      """app (app xs ys) zs
        |    where
        |      app = \xs -> \ys -> case xs of { Nil -> ys | Cons x xs -> Cons x (app xs ys) };
      """.stripMargin
    println("before:")
    println(code)
    println("\nafter:")
    println(Supercompiler(code))
  }

  test("Example 6") {
    val code =
      """sum (map (\x -> mult x x) (int Z n))
        |  where
        |    int = \l -> \r -> case gt l r of { True -> Nil
        |                                     | False -> Cons l (int (S l) r) };
        |    map = \f -> \xs -> case xs of { Nil -> Nil
        |                                  | Cons x xs -> Cons (f x) (map f xs) };
        |    sum = \xs -> case xs of { Nil -> Z
        |                            | Cons x xs -> plus x (sum xs) };
      """.stripMargin
    println("before:")
    println(code)
    println("\nafter:")
    println(Supercompiler(code))
  }

  test("Example 7") {
    val code =
      """length (concat xss)
        |    where
        |      xss = Cons (Cons a (Cons b (Cons c Nil))) (Cons Nil (Cons (Cons d (Cons d Nil)) Nil));
        |      length = \xs -> case xs of { Nil -> Z | Cons x xs -> S (length xs) };
        |      concat = \xss -> case xss of
        |                        { Nil -> Nil
        |                        | Cons xs xss -> app xs (concat xss) };
        |      app = \xs -> \ys -> case xs of { Nil -> ys | Cons x xs -> Cons x (app xs ys) };
      """.stripMargin
    println("before:")
    println(code)
    println("\nafter:")
    println(Supercompiler(code))
  }

  test("Example 8") {
    val code =
      """length (duplicate xs)
        |    where
        |      length = \xs -> case xs of { Nil -> Z | Cons x xs -> S (length xs) };
        |      duplicate = \xs -> case xs of
        |                          { Nil -> Nil
        |                          | Cons x xs -> Cons x (Cons x (duplicate xs)) };
      """.stripMargin
    println("before:")
    println(code)
    println("\nafter:")
    println(Supercompiler(code))
  }

  test("Example 9") {
    val code =
      """length (concat xss)
        |    where
        |      length = \xs -> case xs of { Nil -> Z | Cons x xs -> S (length xs) };
        |      concat = \xss -> case xss of
        |                        { Nil -> Nil
        |                        | Cons xs xss -> app xs (concat xss) };
        |      app = \xs -> \ys -> case xs of { Nil -> ys | Cons x xs -> Cons x (app xs ys) };
      """.stripMargin
    println("before:")
    println(code)
    println("\nafter:")
    println(Supercompiler(code))
  }
}
