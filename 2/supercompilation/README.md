# Supercompiler

## What is it?

Суперкомпиляция -- это метод оптимизации программ. Он состоит в том, что мы делаем абстрактную интерпретацию программы
и пытаемся понять, какие места можно так или иначе оптимизировать. Частным случаем данного подхода является, например,
тот факт, что если в программе нет свободных переменных и функций, то суперкомпилятор превращается в обычный интерпретатор.

Высокоуровнево задачу суперкомпиляции можно сформулировать так -- на вход дана программа, на выход хочется получить
семантически эквивалентную, но оптимизированную ее версию.

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
* Let-блок (let x = e1 in e2).
Let отличается от лямбды подстановкой. В Let e1 вычисляется сразу и результат подставляется в e2, а в лямбду выражение
подставляется целиком, без изменений
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



## Parts
* parser and printer
* reducer
* driver
* folder
* residualizer

## Progress

- [x] parser and printer for lambda calculus only
- [x] reducer for lambda calculus only
- [] parser and printer for all programs
- [] reducer for the whole language
- [] driver
- [] folder
- [] residualizer
