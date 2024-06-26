module App.Lib.Matrix where

matrix :: String
matrix =
  """module Matrix


def add (xss: [[Number]], yss: [[Number]]): [[Number]] =
    // Adds elementwise the numbers of two matrices
    // >>> [[1], [2, 3]], [[4], [5, 6]]
    // >>> [[1, 2], [3]], [[-1, -2], [-3]]
    zipWith (Prelude.add, xss, yss)

def sub (xss: [[Number]], yss: [[Number]]): [[Number]] =
    // Substracts elementwise the numbers of two matrices
    // >>> [[1], [2, 3]], [[4], [5, 6]]
    // >>> [[1, 2], [3]], [[-1, -2], [-3]]
    zipWith (Prelude.sub, xss, yss)

def mult (xss: [[Number]], yss: [[Number]]): [[Number]] =
    // Multiplies elementwise the numbers of two matrices
    // >>> [[1], [2, 3]], [[4], [5, 6]]
    // >>> [[1, 2], [3]], [[-1, -2], [-3]]
    zipWith (Prelude.mult, xss, yss)

def div (xss: [[Number]], yss: [[Number]]): [[Number]] =
    // Divides elementwise the numbers of two matrices
    // >>> [[1], [2]], [[4], [5]]
    // >>> [[1], [3]], [[-1], [-3]]
    zipWith (Prelude.div, xss, yss)

def map (f: A -> B, xss: [[A]]): [[B]] =
    // Maps a unary function over a matrix
    // >>> _ / 2, [[1, 3], [5, 7]]
    // >>> _ ++ "x", [["a"], ["b"]]
    Prelude.map (Prelude.map (f), xss)

def filter (f: A -> Boolean, xss: [[A]]): [[A]] =
    // Uses a predicate to filter a matrix
    // >>> x -> x % 2 == 0, [[1, 2], [3, 4]]
    // >>> x -> length (x) > 2, [["abc"]]
    Prelude.map (Prelude.filter (f), xss)

def reduce (f: B -> A -> B, start: B, xss: [A]): B =
    // Uses a reducer function and initial value to accumulate a value over a matrix
    // >>> 'Prelude.+, 7, [[1, 2], [3, 4]]
    // >>> 'Prelude.++, "hello", [["a"]]
    Prelude.reduce (f, start, Prelude.map (Prelude.fold (f), xss))

def fold (f: A -> A -> A, xss: [[A]]): A =
    // Uses a reducer function to accumulate a value over a non-empty matrix
    // >>> 'Prelude.+, [[1, 2], [3, 4]]
    // >>> 'Prelude.++, [["abc"], ["de"], ["f"]]
    Prelude.fold (f, Prelude.map (Prelude.fold(f), xss))

def zipWith (f: A -> B -> C, xss: [[A]], yss: [[B]]): [[C]] =
    // Maps a binary function over two matrices
    // >>> 'Prelude.-, [[9], [2]], [[5], [6]]
    // >>> 'Prelude.++, [["a"]], [["b"]]
    Prelude.zipWith (Prelude.zipWith (f), xss, yss)

def sum (xss: [[Number]]): Number =
    // Calculates the sum of a matrix of numbers
    // >>> [[1, 2], [3, 4]]
    // >>> [[-1.5, 3.2], [-5.8]]
    reduce (Prelude.add, 0, xss)

def product (xss: [[Number]]): Number =
    // Calculates the product of a matrix of numbers
    // >>> [[1, 2], [3, 4]]
    // >>> [[-1.5, 3.2], [-5.8]]
    reduce (Prelude.mult, 1, xss)

def average (xss: [[Number]]): Number =
    // Calculates the average of a matrix of numbers
    // >>> [[1, 2], [3, 4]]
    // >>> [[-1.5, 3.2], [-5.8]]
    Prelude.div (sum (xss), 2)
"""
