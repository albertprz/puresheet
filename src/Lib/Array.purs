module App.Lib.Array where

array :: String
array =
  """module Array


def add (xs: [Number], ys: [Number]): [Number] =
    // Adds elementwise the numbers of two collections
    // >>> [1, 2, 3], [4, 5, 6]
    // >>> [1, 2, 3], [-1, -2, -3]
    Prelude.zipWith (Prelude.add, xs, ys)

def sub (xs: [Number], ys: [Number]): [Number] =
    // Substracts elementwise the numbers of two collections
    // >>> [1, 2, 3], [4, 5, 6]
    // >>> [1, 2, 3], [-1, -2, -3]
    Prelude.zipWith (Prelude.sub, xs, ys)

def mult (xs: [Number], ys: [Number]): [Number] =
    // Multiplies elementwise the numbers of two collections
    // >>> [1, 2, 3], [4, 5, 6]
    // >>> [1, 2, 3], [-1, -2, -3]
    Prelude.zipWith (Prelude.mult, xs, ys)

def div (xs: [Number], ys: [Number]): [Number] =
    // Divides elementwise the numbers of two collections
    // >>> [1, 2, 3], [4, 5, 6]
    // >>> [1, 2, 3], [-1, -2, -3]
    Prelude.zipWith (Prelude.div, xs, ys)
"""
