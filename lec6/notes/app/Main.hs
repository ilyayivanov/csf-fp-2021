{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Lib
import Data.Semigroup
import Data.Monoid

main :: IO ()
main = someFunc

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6

instance Show SixSidedDie where
    show S1 = "one"
    show S2 = "two"
    show S3 = "three"
    show S4 = "four"
    show S5 = "five"
    show S6 = "six"

class Describable a where
    describe :: a -> String
    describe x = "<Unknown term>"

data Dummy = D1 | D2 deriving (Describable)

data MyNumber = One | Two | Three | Four | Five deriving (Show, Eq, Ord)

squareAndString = show . (^ 2)
sinCos = sin . cos

lr = [1 .. 5] <> [6 .. 10]

combineValues x y = x <> y

x1 = Sum 3
y1 = Sum 4
z1 = x1 <> y1

x2 = Product 3
y2 = Product 4
z2 = x2 <> y2

data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown deriving (Show, Eq)
{-instance Semigroup Color where
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    (<>) a b = if a == b
                      then a
                      else Brown-}

instance Semigroup Color where
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    (<>) a b | a == b = a
             | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
             | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
             | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange

c1 = (Green <> Blue) <> Yellow
c2 = Green <> (Blue <> Yellow)

l1 = [1 .. 10] <> mempty
l2 = mempty <> [1 .. 10]

scon = "abc" `mappend` "def"

s1 = mconcat ["Does"," this"," make"," sense?"]

e0 = mempty :: Sum Integer

e1 = mempty :: Product Integer
