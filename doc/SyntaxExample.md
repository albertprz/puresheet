# Syntax Example

```haskell

sum :: [Int] -> Int
sum (myList) = myList |reduce> (0, '+)

product :: [Int] -> Int
product (myList) = myList |reduce> (1, '*)

findEvens :: [Int] -> [Int]
findEvens = _ |filter> isEven 

divOptions :: Option (Float) -> Option (Float) -> Option (Float)
divOptions (opt1, opt2) = result
  where {
    | result = opt1 |map> '/ |apply> opt2
}


greeting :: Person -> String
greeting (person) =
  switch (person) {
    | Professor (_)                         => "Hello Professor"
    | Student (name) ? isUnknownName (name) => "Nice to meet you, " ++ name
    | Student (name)                        => "Hey, " ++ name
}

head :: [a] -> Option (a)
head ([x, ...]) = Some (x)
head _          = None


addLookup :: Map (String, Int) -> String -> String -> Int
addLookup (env, key1, key2) = 
   if {
     ? Just (val1) <- lookup (env, key1)
     , Just (val2) <- lookup (env, key2)
       = val1 + val2
     ? else = 0
}


-- Cell formula
A1 - B1 * C1

-- Cell range formula
[A1 .. D1] |+| sum ([A2 .. D2]) *| [A3 .. D3]
```
