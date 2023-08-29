# Syntax Example

```haskell

sum :: {Number (a)} :: [a] -> a 
sum (myList) = myList |reduce> (0, '+)

product :: {Number (a)} :: [a] -> a
product (myList) = myList |reduce> (1, '*)

findEvens :: [Int] -> [Int]
findEvens = _ |filter> isEven 

sumOptions :: {Number (a)} :: Option (a) -> Option (a) -> Option (a)
sumOptions (opt1, opt2) = result
  where {
    | result = opt1 |map> '+ |apply> opt2
}


greeting :: Person -> String
greeting (person) =
  switch (person) {
    | Professor (_)                         => "Hello Professor"
    | Student (name) ? isUnknownName (name) => "Nice to meet you, " ++ name
    | Student (name)                        => "Hey, " ++ name
}

head :: [a] -> Option (a)
head ([x, _]) = Some (x)
head _        = None


addLookup :: Map (String, Int) -> String -> String -> Int
addLookup (env, key1, key2) = 
   if {
     ? Just (val1) <- lookup (env, key1)
     , Just (val2) <- lookup (env, key2)
       = val1 + val2
}


-- Cell formula
A1 - B1 * C1

-- Cell range formula
[A1 .. D1] |+| sum ([A2 .. D2]) *| [A3 .. D3]
```
