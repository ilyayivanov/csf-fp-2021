module Main where

import Lib

main :: IO ()
main = someFunc

sayAmountX n =
    case n of
        1 -> "one"
        2 -> "two"
        x -> "a bunch of " ++ (show x) ++ " size"

sayAmountGen n =
    case n of
        1 -> "one"
        2 -> "two"
        _ -> "a bunch"

sayAmount 1 = "one"
sayAmount 2 = "two"
sayAmount _ = "a bunch"

sayAmountR 1 = "one"
sayAmountR 2 = "two"
sayAmountR 3 = "three"

getFirst (a, b) = a
getSecond (a, b) = b

getHead (x : xs) = x -- x : xs сопоставляется только с непустым списком!
getHead _ = error "No head for empty list"

getTail (x : xs) = xs
getTail _ = error "No tail for empty list"

isEmpty [] = True
isEmpty _ = False

groupBy3 n
    | n `mod` 3 == 0 = "First group"
    | n `mod` 3 == 1 = "Second group"
    | otherwise = "Third group"

avgGrade x
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 0.59 = 'D'
    | y < 0.59 = 'F'
    where y = x / 100

fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacci (n - 2) + fibonacci (n - 1)

fibonacciImpr n =
    helper n 0 1
    where
        helper n acc1 acc2
            | n == 0 = acc1
            | n == 1 = acc2
            | otherwise = helper (n - 1) acc2 (acc1 + acc2)

myLength [] = 0
myLength xs = 1 + myLength (tail xs)

myLength2 [] = 0
myLength2 (x : xs) = 1 + myLength2 xs

myLength3 xs = lengthHlp 0 xs
    where
        lengthHlp acc [] = acc
        lengthHlp acc (x : xs) = lengthHlp (acc + 1) xs

myTake _ [] = []
myTake 0 _ = []
myTake n (x : xs) = x : rest
    where rest = myTake (n - 1) xs

myTake' :: (Eq a1, Num a1) => a1 -> [a2] -> [a2]
myTake' _ [] = []
myTake' 0 _ = []
myTake' n (x : xs) = x : myTake2 (n - 1) xs

myTake2 n xs = reverse $ takeHlp n xs []
    where
        takeHlp 0 _ acc = acc
        takeHlp _ [] acc = acc
        takeHlp n (x : xs) acc = takeHlp (n - 1) xs (x : acc)

finiteCycle (first : rest) = first : rest ++ [first]

myCycle [] = error "Empty list"
myCycle (first : rest) = first : myCycle (rest ++ [first])

m1 = map snd [(1, 'a'), (2, 'b'), (3, 'c')]
m2 = map reverse ["abc", "d", "defghi"]
m3 = map ("a " ++) ["train","plane","boat"]

f1 = filter odd [1 .. 10]
f2 = filter (\s -> head s == 'b') ["angle", "bread", "square", "ball"]

sum1 = foldl (+) 0 [1, 2, 3, 4, 5]
sum2 = foldr (+) 0 [1, 2, 3, 4, 5]

diff1 = foldl (-) 0 [1, 2, 3, 4, 5]
diff2 = foldr (-) 0 [1, 2, 3, 4, 5]
