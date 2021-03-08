module Main where

import Lib

main :: IO ()
main = someFunc

getRequestURL host apiKey resource id = host ++
    "/" ++
    resource ++
    "/" ++
    id ++
    "?token=" ++
    apiKey

getMyAppRequest = getRequestURL "www.myapp.com"
getMyAppSecuredRequest = getRequestURL "www.myapp.com" "token_20210228-1710"
getMyAppSecuredBookRequest = getRequestURL "www.myapp.com" "token_20210228-1710" "book"
getMyAppSecured2BookRequest = getRequestURL "www.myapp.com" "token_20210228-1755" "book"

getMyAppBookRequest bookId apiKey = getRequestURL "www.myapp.com" apiKey "book" bookId

multuplyBy5V1 = (* 5)
multuplyBy5V2 = (*) 5

divideBy6 = (/ 6)
divide6 = (/) 6

ifEven f n =
    if even n
        then f n
        else n

inc n = n + 1
double n = n * 2
square n = n ^ 2

ifEvenInc n = ifEven inc n
ifEvenDouble n = ifEven double n
ifEvenSquare n = ifEven square n

buildIfEven f = (\x -> ifEven f x)

ifEvenInc2 = buildIfEven inc
ifEvenDouble2 = buildIfEven double
ifEvenSquare2 = buildIfEven square

buildIfEven2 f = ifEven f

buildIfEven3 = ifEven

ifEvenInc3 = ifEven inc
ifEvenDouble3 = ifEven double
ifEvenSquare3 = ifEven square

emptyList = []
singleton = 1 : []
pair = 2 : singleton
chain = 1 : 2 : 3 : 4 : 5 : []
chain2 = [1, 2, 3, 4, 5]
list2 = (:) 3 [2, 1]

s1 = "abcdef"
s2 = ['a', 'b', 'c', 'd', 'e', 'f']

s3 = 'a' : "bcdef"
s4 = "abc" ++ "def"

is1 = [1, 2, 3] ++ [4, 5]

l1 = [1 .. 10]
l2 = [1, 3 .. 10]
l3 = [10, 8 .. -10]
l4 = [1 .. ]
l5 = [1, 3 .. ]

res = [1 .. 10] `zip` ['a' .. 'z']

myGCD a b =
    if remainder == 0
        then b
        else myGCD b remainder
    where remainder = a `mod` b
