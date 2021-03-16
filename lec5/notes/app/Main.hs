module Main where

import Lib

main :: IO ()
main = someFunc

x = 5

y = 6 :: Int

squareSum :: Double -> Double -> Double
squareSum a b = (a + b) ^ 2

fraction1 = 15 + read "0.33"

fraction2 :: Double
fraction2 = 15 + read "0.33"

type FirstName = String
type LastName = String
type Age = Int
type Height = Double
type Weight = Double
type Patient = (FirstName, LastName, Age, Height, Weight)

firstName :: Patient -> FirstName
firstName (fName, _, _, _, _) = fName

height :: Patient -> Height
height (_, _, _, h, _) = h

p1 :: Patient
p1 = ("Ivan", "Sidorov", 20, 185.6, 80)

p2 = ("Petr", "Sergeev", 21 :: Int, 183.0, 75.0)

p3 = ("Petr", "Sergeev", 21, 183.0, 75.0)

-- data Bool = False | True -- Определение булевского типа в стандартной бибилиотеке

data Colour = Red | Green | Blue
c1 = Blue
с2 = Red

data Student = Stdnt String String String Int Int String
std1 = Stdnt "Antonov" "Petr" "Vyacheslavovich" 102973 2 "2.2"

data Vehicle = Vehicle String String Double Int

data Sum1 = Sum11 | Sum12 | Sum13
data Sum2 = Sum21 | Sum22
data Sum3 = Sum31 | Sum32 | Sum33 | Sum34
data ProdOfSum = ProdOfSum Sum1 Sum2 Sum3 -- (Sum11 | Sum12 | Sum13) (Sum21 | Sum22) (Sum31 | Sum32 | Sum33 | Sum34)

data Prod1 = Prd1 Int Bool Double
data Prod2 = Prd2 String (Bool, Double)
data SumOfProd = Dat1 | Dat2 Prod1 String | Dat3 String Prod2

type MiddleName = String
data Name = Name FirstName LastName
    | NameWithMiddle FirstName MiddleName LastName

data Gender = Mail | Femail

data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType

data PatientData = PatientData {
    name :: Name,
    gender :: Gender,
    age :: Int,
    pHeight :: Double,
    pWeight :: Double,
    bloodType :: BloodType
}

pd1 = PatientData {
    gender = Femail,
    bloodType = BloodType AB Pos,
    name = Name "Alice" "Braun",
    age = 25,
    pHeight = 165.3,
    pWeight = 62.0
}

n1 = name pd1
h1 = pHeight pd1

data CustomList a = Nil | Cons a (CustomList a) -- Рекурсивное определени типа

l1 :: CustomList Int
l1 = Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))

l2 :: CustomList Int
l2 = Cons 1 $ Cons 2 $ Cons 3 $ Cons 4 Nil

cHead :: CustomList a -> a
cHead Nil = error "Empty list"
cHead (Cons x tail) = x

cTakeRev :: Int -> CustomList a -> CustomList a
cTakeRev n list = helper n list Nil
    where
        helper 0 _ acc = acc
        helper _ Nil acc = acc
        helper m (Cons x tail) acc = helper (m - 1) tail (Cons x acc)

customHead = cHead (cTakeRev 3 l2)

data BTree a = Empty | Leaf a | Node a (BTree a) (BTree a)

b1 :: BTree Int
b1 = Node 10 (Node 6 (Node 3 Empty (Leaf 5)) (Leaf 8)) (Leaf 16)

depth :: BTree a -> Int
depth Empty = 0
depth (Leaf n) = 1
depth (Node x l r) = 1 + max (depth l) (depth r)
