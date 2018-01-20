# Supercompiler

## What is it?

Суперкомпиляция -- это метод оптимизации программ. Он состоит в том, что мы делаем абстрактную интерпретацию программы
и пытаемся понять, какие места можно так или иначе оптимизировать. Частным случаем данного подхода является, например,
тот факт, что если в программе нет свободных переменных и функций, то суперкомпилятор превращается в обычный интерпретатор.

Высокоуровнево задачу суперкомпиляции можно сформулировать так -- на вход дана программа, на выход хочется получить
семантически эквивалентную, но оптимизированную ее версию.

## Links

http://www.mathnet.ru/links/c00fdb2724e653d378355889f52402bf/ps10.pdf

## Language

Программа на нашем языке представляет из себя:

```
expression_0
    where
        function_1_defenition = expression_1;
        function_2_defenition = expression_2;
        ...
        function_n_defenition = expression_n;
```

Результат работы -- это вычисление `expression_0`

Конструкции, которые встречаются в выражениях:
* Переменные (как связанные, так и нет)
* Функции (вызовы функций; как связанных, так и нет)
* Лямды (\x -> x)
* Аппликация (e1 e2)
* Конструкторы (Constr a b c, где a b c -- переменные; переменных может быть 0)
* Case-блок (Case e of {Constr1 ..  -> .. | Constr2 ..  -> .. | ...}).
Case, в общем, такой же как в Haskell (или как match в Scala)

## Program Examples

Небольшие примеры программ на нашем языке:

_Example 1_

```
sum (map (\x -> mult x x) (int Z n))
  where
    int = \l -> \r -> case gt l r of { True -> Nil
                                     | False -> Cons l (int (S l) r) };
    map = \f -> \xs -> case xs of { Nil -> Nil
                                  | Cons x xs -> Cons (f x) (map f xs) };
    sum = \xs -> case xs of { Nil -> Z
                            | Cons x xs -> plus x (sum xs) };
```

Эта программа просто считает сумму квадратов чисел от 0 до n.
В идеале, после работы нашего суперкомпилятора, мы должны получить что-то такое

```
f0 Z
  where
    f0 = \y0 -> case gt y0 n of { True -> Z
                                | False -> plus (mult y0 y0) (f0 (S y0)) };
```

_Example 2_

```
id (apply id (app xs ys))
  where
    id = \x -> x;
    apply = \f -> \x -> f x;
    app = \xs -> \ys -> case xs of { Nil -> ys
                                   | Cons x xs -> Cons x (app xs ys) };
```

В идеале, после работы нашего суперкомпилятора, мы должны получить что-то такое

```
f3 xs
  where
    f3 = \y0 -> case y0 of { Nil -> ys
                           | Cons x0 x1 -> Cons x0 (f3 x1) };
```

В данном случае, компилятор заметил, что функция `app` паттер-матчится только по первому аргументу, и сделал ее рекурсивной

## Examples of work

This results are produced by `CompilerTest` in `test/` folder

```
     Example 1:

before:
foo (case Nil of {NotNil -> Nil | Nil -> case (\x . x) NotNil of {NotNil -> Nil}})
  where
    foo = \x . case x of {NotNil -> Fail | Nil -> Success};


after:
Success

====================

     Example 2:

before:
snd (Cons 1 (snd (Cons 2 3)))
    where
      snd = \x . case x of { Cons a b -> b };


after:
3

====================

     Example 3:

before:
foo 1 xs
    where
      foo = \x . \xs . case xs of {Cons a b -> foo x b
                                    | Nil -> x};


after:
(f0 xs)
  where
    f0 = (\x0 -> case x0 of {Cons x1 x2 -> (f0 x2) | Nil -> 1});

====================

     Example 4:

before:
id (apply id (app xs ys))
  where
    id = \x -> x;
    apply = \f -> \x -> f x;
    app = \xs -> \ys -> case xs of { Nil -> ys
                                   | Cons x xs -> Cons x (app xs ys) };


after:
(f0 xs)
  where
    f0 = (\x0 -> case x0 of {Cons x1 x2 -> (Cons x1 (f0 x2)) | Nil -> ys});

====================

     Example 5:

before:
app (app xs ys) zs
    where
      app = \xs -> \ys -> case xs of { Nil -> ys | Cons x xs -> Cons x (app xs ys) };


after:
(f0 xs)
  where
    f0 = (\x0 -> case x0 of {Cons x1 x2 -> (Cons x1 (f0 x1)) | Nil -> (f1 ys)});
    f1 = (\x0 -> case x0 of {Cons x1 x2 -> (Cons x1 (f1 x2)) | Nil -> zs});

====================

     Example 6:

before:
sum (map (\x -> mult x x) (int Z n))
  where
    int = \l -> \r -> case gt l r of { True -> Nil
                                     | False -> Cons l (int (S l) r) };
    map = \f -> \xs -> case xs of { Nil -> Nil
                                  | Cons x xs -> Cons (f x) (map f xs) };
    sum = \xs -> case xs of { Nil -> Z
                            | Cons x xs -> plus x (sum xs) };


after:
(f0 Z)
  where
    f0 = (\x0 -> case ((gt x0) n) of {False -> ((plus ((mult x0) x0)) (f0 (S x0))) | True -> Z});

====================

     Example 7:

before:
length (concat xss)
    where
      xss = Cons (Cons a (Cons b (Cons c Nil))) (Cons Nil (Cons (Cons d (Cons d Nil)) Nil));
      length = \xs -> case xs of { Nil -> Z | Cons x xs -> S (length xs) };
      concat = \xss -> case xss of
                        { Nil -> Nil
                        | Cons xs xss -> app xs (concat xss) };
      app = \xs -> \ys -> case xs of { Nil -> ys | Cons x xs -> Cons x (app xs ys) };


after:
(S (S (S (S (S Z)))))

====================

     Example 8:

before:
length (duplicate xs)
    where
      length = \xs -> case xs of { Nil -> Z | Cons x xs -> S (length xs) };
      duplicate = \xs -> case xs of
                          { Nil -> Nil
                          | Cons x xs -> Cons x (Cons x (duplicate xs)) };


after:
(f0 (f1 xs))
  where
    f0 = (\x0 -> case x0 of {Cons x1 x2 -> (S (f0 x2)) | Nil -> Z});
    f1 = (\x0 -> case x0 of {Cons x1 x2 -> (Cons x1 (Cons x1 (f1 x2))) | Nil -> Nil});

====================

     Example 9:

before:
length (concat xss)
    where
      length = \xs -> case xs of { Nil -> Z | Cons x xs -> S (length xs) };
      concat = \xss -> case xss of
                        { Nil -> Nil
                        | Cons xs xss -> app xs (concat xss) };
      app = \xs -> \ys -> case xs of { Nil -> ys | Cons x xs -> Cons x (app xs ys) };


after:
case ((\x0 -> (f1 x0)) xss) of {Cons x0 x1 -> (S (f0 x1)) | Nil -> Z}
  where
    f0 = (\x0 -> case x0 of {Cons x1 x2 -> (S (f0 x2)) | Nil -> Z});
    f1 = (\x0 -> case x0 of {Cons x1 x2 -> ((f2 x1) x2) | Nil -> Nil});
    f2 = (\x0 -> (\x1 -> case x0 of {Cons x2 x3 -> (Cons x2 ((f2 x3) x3)) | Nil -> (f1 x1)}));
```