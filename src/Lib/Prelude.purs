module App.Lib.Prelude where

prelude :: String
prelude =
  """module Prelude

op <| = apply R0

op |> = applyFlipped L1

op || = or R2

op && = and R3

op == = eq L4

op != = notEq L4

op > = gt L4

op >= = gtOrEq L4

op < = lt L4

op >= = ltOrEq L4

op ++ = append R5

op +: = cons R6

op :+ = snoc L7

op + = add L8

op |+| = Array.add L8

op ||+|| = Matrix.add L8

op - = sub L9

op |-| = Array.sub L9

op ||-|| = Matrix.sub L9

op <<< = compose R9

op >>> = composeFlipped R9

op * = mult L10

op |*| = Array.mult L10

op ||*|| = Matrix.mult L10

op / = div L11

op |/| = Array.div L11

op ||/|| = Matrix.div L11

op // = intDiv L11

op % = mod L11


def id (x: A): A =
    // Returns its argument
    // >>> []
    // >>> true
    x

def const (x: A): B -> A =
    // Returns its first argument
    // >>> 2, 7
    // >>> true, false
    y -> x

def apply (f: A -> B, x: A): B =
    // Applies a unary function to an argument
    // >>> _ * 3, 6
    // >>> head, ['a', 'b', 'c']
    f (x)

def compose (f: A -> B, g: B -> C): A -> C =
    // Composes two functions
    // >>> _ * 3, _ + 1, 2
    // >>> head, tail, [2, 4, 6]
    x -> f (g (x))

def flip (f: A -> B -> C): B -> A -> C =
    // Flips the arguments of the function
    // >>> '-, 2, 1
    // >>> '++, "hel", "lo"
    (x, y) -> f (y, x)

def map (f: A -> B, xs: [A]): [B] =
    // Maps a unary function over a collection
    // >>> _ / 2, [1, 3, 5, 7]
    // >>> _ ++ "x", ["a", "c", "c"]
    go (f, xs, []) where {
        | go (f, xs, acc) = switch (xs) {
            | [] => acc
            | [ xs @ ... , x ] => recur (f, xs, f (x) +: acc)
        }
    }

def filter (f: A -> Boolean, xs: [A]): [A] =
    // Uses a predicate to filter a collection
    // >>> x -> x % 2 == 0, [1, 2, 3, 4]
    // >>> x -> length (x) > 2, ["abc", "f"]
    go (f, xs, []) where {
        | go (f, xs, acc) = switch (xs) {
            | []                        => acc
            | [ x, xs @ ... ] ? f(x)    => recur (f, xs, x +: acc)
                            ? otherwise => recur (f, xs, acc)
        }
    }

def reduce (f: B -> A -> B, start: B, xs: [A]): B =
    // Uses a function and an initial value to accumulate over a collection
    // >>> '+, 7, [1, 2, 3, 4]
    // >>> '++, "hello", ["abc", "f"]
    go (f, start, xs, start) where {
        | go (f, start, xs, acc) = switch (xs) {
            | []             => acc
            | [ x, xs @ ... ] => recur (f, start, xs, f(acc, x))
        }
    }

def fold (f: A -> A -> A, xs: [A]): A =
    // Uses a reducer function to accumulate a value over a non-empty collection
    // >>> '+, [1, 2, 3, 4]
    // >>> '++, ["abc", "de", "f"]
    switch (xs) {
        | [ x, xs @ ... ] => reduce (f, x, xs)
    }

def zipWith (f: A -> B -> C, xs: [A], ys: [B]): [C] =
    // Maps a binary function over two collections
    // >>> '-, [1, 2, 3], [5, 6, 7]
    // >>> '++, ["a", "b"], ["c", "d"]
    go (f, xs, ys, []) where {
        | go (f, xs, ys, acc) = cond {
            ? isEmpty (xs) || isEmpty (ys) => acc
            ? otherwise => recur (f, tail (xs), tail (ys),
                                 f (head (xs), head (ys)) +: acc)
        }
    }

def iterate (f: A -> A, start: A, n: Int): [A] =
    // Generates a collection by applying a function n times with an initial value
    // >>> _ * 2, 1, 6
    // >>> _ ++ "x", "a", 3
    go (f, start, n, []) where {
        | go (f, start, n, acc) = cond {
            ? n == 0     => reverse (acc)
            ? otherwise => recur (f, f (start), n - 1, start +: acc)
        }
    }

def isEmpty (xs: [A]): Boolean =
    // Checks if a collection is empty
    // >>> "asd"
    // >>> []
    switch (xs) {
        | [] => true
        | "" => true
        | _  => false
    }

def isNotEmpty (xs: [A]): Boolean =
    // Checks if a collection is not empty
    // >>> "asd"
    // >>> []
    not (isEmpty (xs))

def all (f: A -> Boolean, xs: [A]): Boolean =
    // Checks if a predicate succeeds for all the elements in a collection
    // >>> _ > 7, []
    // >>> _ > 7, [3, 6, 9]
    reduce (and, true, map (f, xs))

def any (f: A -> Boolean, xs: [A]): Boolean =
    // Checks if a predicate succeeds for at least one element in a collection
    // >>> _ > 7, []
    // >>> _ > 7, [3, 6, 9]
    reduce (or, false, map (f, xs))


def sum (xs: [Number]): Number =
    // Calculates the sum of a collection of numbers
    // >>> [1, 2, 3, 4]
    // >>> [-1.5, 3.2, -5.8]
    reduce (add, 0, xs)

def product (xs: [Number]): Number =
    // Calculates the product of a collection of numbers
    // >>> [1, 2, 3, 4]
    // >>> [-1.5, 3.2, -5.8]
    reduce (mult, 1, xs)

def average (xs: [Number]): Number =
    // Calculates the average of a collection of numbers
    // >>> [1, 2, 3, 4]
    // >>> [-1.5, 3.2, -5.8]
    sum (xs) / 2
"""
