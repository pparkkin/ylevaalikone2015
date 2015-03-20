{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as T

import Data.List (group, sort, groupBy, sortBy, transpose)
import Data.Function (on)
import Data.List.Utils (flipAL)
import Data.Maybe (catMaybes)
import Control.Arrow ((&&&), second)

import Text.Printf (printf)

-- Try to convert a multiple choice answer to a Float
toValue :: T.Text -> Maybe Float
toValue t
    | T.isPrefixOf "t\195\8364ysin s" t = Just 2
    | T.isPrefixOf "jokseenkin s" t     = Just 1
    | T.isPrefixOf "jokseenkin e" t     = Just (-1)
    | T.isPrefixOf "t\195\8364ysin e" t = Just (-2)
    | otherwise                         = Nothing

-- Get a field by number from a single line from the file,
-- and try to convert to Float
getFieldValue :: Int -> T.Text -> Maybe Float
getFieldValue n = toValue . getField n

-- Get a field by number from a single line from the file
getField :: Int -> T.Text -> T.Text
getField n t = T.splitOn ";" t !! n

-- Lookup all the values for a single key
-- Could also be catMaybes $ lookup a $ groupAL
lookupAll :: (Eq a) => [(a, b)] -> a -> [b]
lookupAll [] _ = []
lookupAll ((k, v):al) k'
    | k == k' = v : lookupAll al k'
    | otherwise = lookupAll al k'

-- Group all the values for every key into a single element
groupAL :: (Eq a, Eq b) => [(a, b)] -> [(a, [b])]
groupAL = flipAL . map invert
    where invert (a, b) = (b, a)

-- Is this a header for a multiple choice question?
isMultiHeader :: T.Text -> Bool
isMultiHeader t
    | length (T.splitOn "|" t) /= 2 = False
    | otherwise = d /= "kommentti"
        where d = T.splitOn "|" t !! 1

-- Select the columns that should have useful data
selectColumns :: [[T.Text]] -> [[T.Text]]
selectColumns [] = []
selectColumns (c:cs)
    | (head c) == "puolue" = c : selectColumns cs
    | isMultiHeader (head c) = c : selectColumns cs
    | otherwise = selectColumns cs

variance :: [Float] -> Float
variance fs = (sum (map (\f -> (f - average) ^ 2) fs)) / n
    where average = (sum fs) / n
          n = fromIntegral (length fs)

mode :: [Float] -> Float
mode fs = head $ head md
    where md = reverse $ sortBy (compare `on` length) od
          od = group $ sort fs

-- Make a list of floats into one
-- Try to do it in a smart way
reduce :: [Float] -> Float
reduce [] = 0.0
reduce vs = mode vs

-- Take the party column and one value column, and combine them
-- into an AL with party and list of values
pvZippy :: [T.Text] -> [Maybe Float] -> [(T.Text, [Float])]
pvZippy ps vs = let ps' = tail ps -- drop header
                    vs' = tail vs -- ditto
                    pvs = groupAL $ zip ps' vs'
                in map (second catMaybes) pvs

-- Take an AL of party, list of values, and combine the values for
-- a party into one
combine :: [(T.Text, [Float])] -> [(T.Text, Float)]
combine pvs = map (second reduce) pvs

-- Split into columns
toCols :: T.Text -> [[T.Text]]
toCols s = transpose $ map (T.splitOn ";") (T.lines s)

-- Merge column values into lists
-- Assumes same order
mergeVK :: [[(T.Text, Float)]] -> [(T.Text, [Float])]
mergeVK = concat . (map groupAL) . transpose

-- Calculate distance between two parties
distance :: [Float] -> [Float] -> Float
distance xs ys = sqrt $ sum $ map (\(x, y) -> (x - y) ^ 2) $ zip xs ys

-- Calculate distances to others
distances :: (T.Text, [Float]) -> [(T.Text, [Float])] -> (T.Text, [Float])
distances (p, xs) ys = (p, map ((distance xs) . snd) ys)

compute :: T.Text -> [(T.Text, [Float])]
compute s = let cols = selectColumns $ toCols s
                pcol = head cols
                vcols = map (map toValue) $ tail cols
                allz = map (pvZippy pcol) vcols
                merged = mergeVK $ map combine allz
                goodz = filter interesting merged
            in map (\g -> distances g goodz) goodz
        where interesting (_, vs) = 50 < (length (filter (/= 0.0) vs))

printableName :: T.Text -> T.Text
printableName n = if T.isPrefixOf "Suomen " n
    then T.drop 7 n
    else n

printRow :: (T.Text, [Float]) -> IO ()
printRow (p, vs) = do
    putStr $ printf "%5s " (take 5 (T.unpack (printableName p)))
    mapM_ (\v -> putStr (printf "%5.2f " v)) vs
    putStrLn ""

printHeader :: [(T.Text, [Float])] -> IO ()
printHeader t = do
    putStr "      "
    mapM_ (\r -> putStr $ printf "%5s " (take 5 (T.unpack (printableName (fst r))))) t
    putStrLn ""

printTable :: [(T.Text, [Float])] -> IO ()
printTable t = do
    printHeader t
    mapM_ printRow t

main :: IO ()
main = do
    putStrLn "Hello, World!"
    args <- getArgs
    content <- C8.readFile (head args)
    printTable $ compute $ decodeUtf8 content

