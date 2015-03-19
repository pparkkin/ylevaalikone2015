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

toValue :: T.Text -> Maybe Float
toValue t
    | T.isPrefixOf "t\195\8364ysin s" t = Just 2
    | T.isPrefixOf "jokseenkin s" t     = Just 1
    | T.isPrefixOf "jokseenkin e" t     = Just (-1)
    | T.isPrefixOf "t\195\8364ysin e" t = Just (-2)
    | otherwise                         = Nothing

getFieldValue :: Int -> T.Text -> Maybe Float
getFieldValue n = toValue . getField n

getField :: Int -> T.Text -> T.Text
getField n t = T.splitOn ";" t !! n

lookupAll :: (Eq a) => [(a, b)] -> a -> [b]
lookupAll [] _ = []
lookupAll ((k, v):al) k'
    | k == k' = v : lookupAll al k'
    | otherwise = lookupAll al k'

groupAL :: (Eq a, Eq b) => [(a, b)] -> [(a, [b])]
groupAL = flipAL . map invert
    where invert (a, b) = (b, a)

isMultiHeader :: T.Text -> Bool
isMultiHeader t
    | length (T.splitOn "|" t) /= 2 = False
    | otherwise = d /= "kommentti"
        where d = T.splitOn "|" t !! 1

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

reduce :: [Float] -> Float
reduce [] = 0.0
reduce vs = mode vs

compute :: T.Text -> [(T.Text, Float)]
compute s = let cols = selectColumns $ transpose $ map (T.splitOn ";") (T.lines s)
                party = tail $ head cols
                vals = map (map toValue) $ tail cols
                -- TODO: Do the same for all cols in vals, and combine 
                pvals = groupAL $ zip party (tail $ head vals)
            in map (second (reduce . catMaybes)) pvals

main :: IO ()
main = do
    putStrLn "Hello, World!"
    args <- getArgs
    content <- C8.readFile (head args)
    print $ compute $ decodeUtf8 content

