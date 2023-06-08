# Syntax Example

```haskell

sum :: Number a => [a] -> a 
sum (myList) = myList |reduce> (0, '+)

product :: Number a => [a] -> a
product (myList) = myList |reduce> (1, '*)

sumOptions :: Number a => Option (a) -> Option (a) -> Option (a)
sumOptions (opt1, opt2) = opt1 |map> '+ |apply> opt2

findEvens :: [Int] -> [Int]
findEvens nums = isEven <filter| nums


head :: [a] -> Option (a)
head (myList) =
  case myList of
    | [x, _] => Some x
    | _      => None


greeting :: Person -> String
greeting (person) =
  case person of
    | Professor(_)                        => "Hello Professor"
    | Student(name) ? isUnknownName(name) => "Nice to meet you, " ++ name
    | Student(name)                       => "Hey, " ++ name"
    

-- Cell range formula
[$A4 .. $D4] = [$A1 .. $D1] |+| (sum [$A2 .. $D2]) *| [$A3 .. $D3]
```
