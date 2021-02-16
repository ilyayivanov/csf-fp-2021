module Main where

import Lib (someFunc)
import Data.List

main :: IO ()
main = someFunc

func1 x y z = x * y + z

var1 = 1

var2 = 4

calcChange owed given =
    if given - owed > 0
        then given - owed
        else 0

calcChangeImproved owed given =
    if change > 0 && val == 1
        then change
        else 0
    where
        change = given - owed
        val = 1
        val3 = 4
        val4 = "123123123"

sumSquareOrSquareSum x y =
    if sumSquare > squareSum
        then sumSquare
        else squareSum
    where
        sumSquare = (x + y) ^ 2
        squareSum = x ^ 2 + y ^ 2

sumSquareOrSquareSum1 x y =
    if (x ^ 2 + y ^ 2) > ((x + y) ^ 2)
        then (x ^ 2 + y ^ 2)
        else (x + y) ^ 2

body sumSquare squareSum =
    if sumSquare > squareSum
        then sumSquare
        else squareSum

lSumSquareOrSquareSum x y =
    (\sumSquare squareSum ->
        if sumSquare > squareSum
            then sumSquare
            else squareSum) (x ^ 2 + y ^ 2) ((x + y) ^ 2)

res = (\x y ->
        (\sumSquare squareSum ->
            if sumSquare > squareSum
                then sumSquare
                else squareSum) (x ^ 2 + y ^ 2) ((x + y) ^ 2)) 3 4

res1 = (\x y ->
        (\sumSquare squareSum ->
            if sumSquare > squareSum
                then sumSquare
                else squareSum) (x ^ 2 + y ^ 2) ((x + y) ^ 2)) 5 (-4)

pSumSquareOrSquareSum x y =
    let sumSquare = (x ^ 2 + y ^ 2)
        squareSum = (x + y) ^ 2
    in
        if sumSquare > squareSum
            then sumSquare
            else squareSum

overwrite x =
    let x = 2
    in
        let x = 3
        in
            let x = 4
            in
                x

ifEvenInc n =
    if even n
        then n + 1
        else n

ifEvenDouble n =
    if even n
        then n * 2
        else n

ifEvenSquare n =
    if even n
        then n ^ 2
        else n

ifEven n doThis =
    if even n
        then doThis n
        else n

sIfEvenInc n = ifEven n (\x -> x + 1)
sIfEvenDouble n = ifEven n (\x -> x * 2)
sIfEvenSquare n = ifEven n (\x -> x ^ 2)

pIfEvenInc n = ifEven n (+ 1)
pIfEvenDouble n = ifEven n (* 2)
pIfEvenSquare n = ifEven n (^ 2)

author = ("Will", "Kurt")

names = [("Ian", "Curtis"),
    ("Bernard","Sumner"),
    ("Peter", "Hook"),
    ("Stephen","Morris")]

compareLastNames name1 name2 =
    if lastName1 > lastName2
        then GT
        else if lastName1 < lastName2
                then LT
                else EQ
    where
        lastName1 = snd name1
        lastName2 = snd name2
