module Main where

import Lib
import Data.Maybe
import Control.Monad

import qualified Data.Map as Map

main :: IO ()
main = someFunc

data Entity = Entity {
    entityId :: Int,
    name :: String,
    secretWord :: String
  } deriving (Show)

requestEntity :: Int -> Maybe Entity
requestEntity id =
  if even id
    then
      Just Entity {
            entityId = id,
            name = "Some Entity",
            secretWord = "abcdef"
          }
    else Nothing

ids = [1 .. 20]

maybeEntities = map requestEntity ids

justEntities = filter isJust maybeEntities
entities = map fromJust justEntities

entities2 = mapMaybe requestEntity ids

data EntityView = EntityView {
    entityViewId :: Int,
    entityViewName :: String
} deriving (Show)

toEntityView :: Entity -> EntityView
toEntityView entity = EntityView {
    entityViewId = entityId entity,
    entityViewName = name entity
}

entityViews = toEntityView <$> entities2

type LatLong = (Double, Double)
locationDB :: Map.Map String LatLong
locationDB = Map.fromList [
        ("Arkham", (42.6054, -70.7829)),
        ("Innsmouth", (42.8250, -70.8150)),
        ("Carcosa", (29.9714, -90.7694)),
        ("New York", (40.7776, -73.9691))
    ];

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180
latLongToRads :: LatLong -> (Double,Double)
latLongToRads (lat,long) = (rlat,rlong)
    where
        rlat = toRadians lat
        rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
    where
        (rlat1, rlong1) = latLongToRads coords1
        (rlat2,rlong2) = latLongToRads coords2
        dlat = rlat2 - rlat1
        dlong = rlong2 - rlong1
        a = (sin (dlat/2))^2 + cos rlat1 * cos rlat2 * (sin (dlong/2))^2
        c = 2 * atan2 (sqrt a) (sqrt (1-a))
        earthRadius = 3961.0

newYorkCoord = Map.lookup "New York" locationDB
voronezhCoord = Map.lookup "Voronezh" locationDB

haversineMaybe :: Maybe LatLong -> Maybe LatLong -> Maybe Double
haversineMaybe Nothing _ = Nothing
haversineMaybe _ Nothing = Nothing
haversineMaybe (Just val1) (Just val2) = Just (haversine val1 val2)

carcosaCoord = Map.lookup "Carcosa" locationDB

distFromNewYorkToX = haversine <$> newYorkCoord

distFromNewYourToCarcosa = distFromNewYorkToX <*> carcosaCoord

dist2 = haversine <$> newYorkCoord <*> carcosaCoord

data User = User {
    userName :: String,
    gamerId :: Int,
    score :: Int
} deriving (Show);

u1 = User {userName = "Tom", gamerId = 1, score = 99}
u2 = User "Mike" 2 85

dataFromDB = [
        (Just "Kyle", Just 5, Just 70),
        (Just "Robbie", Just 6, Nothing)
    ];

createUser :: (Maybe String, Maybe Int, Maybe Int) -> Maybe User
createUser (userName, gamerId, score) = User <$> userName <*> gamerId <*> score

users = mapMaybe createUser dataFromDB

functorPlus = (6 +) <$> Just 5
applicativePlus = pure (6 +) <*> Just 5

helloMonad = putStrLn "It's a not very useful function demonstrating monad's operators chaining"
greeting = putStrLn "Enter a String and we'll echo it!"

getStringFromConsole = getLine

echoVerbose :: IO ()
echoVerbose =
    helloMonad >>
    (
        greeting >>
          (getStringFromConsole >>= putStrLn)
    )

askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName = askForName >>
    getLine >>=
    (\name -> return (nameStatement name)) >>=
    putStrLn

helloNameDo :: IO ()
helloNameDo = do
    askForName
    name <- getLine
    putStrLn (nameStatement name)

powersOfTwo :: Int -> [Int]
powersOfTwo n = do
    value <- [1 .. n]
    return (2 ^ value)

powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
    value <- [1 .. n]
    let powersOfTwo = 2 ^ value
    let powersOfThree = 3 ^ value
    return (powersOfTwo, powersOfThree)

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
    evenValue <- [2, 4 .. n]
    oddValue <- [1, 3 .. n]
    return (evenValue, oddValue)

evensGuard :: Int -> [Int]
evensGuard n = do
    value <- [1 .. n]
    guard (even value)
    return value

powersOfTwoCompr :: Int -> [Int]
powersOfTwoCompr n = [value ^ 2 | value <- [1 .. n]]

powersOfTwoAndThreeCompr :: Int -> [(Int, Int)]
powersOfTwoAndThreeCompr n = [(powersOfTwo, powersOfThree) |
    value <- [1 .. n],
    let powersOfTwo = 2 ^ value,
    let powersOfThree = 3 ^ value]

evensGuardCompr :: Int -> [Int]
evensGuardCompr n = [ value | value <- [1 .. n], even value]
