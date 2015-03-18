{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as T

import Data.List (groupBy, sortBy)
import Data.Function (on)
import Data.List.Utils (addToAL)
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

groupAL :: (Eq a) => [(a, b)] -> [(a, [b])]
groupAL [] = []
groupAL ((k, v):al) = case lookup k r of
        Just vs -> addToAL r k (v:vs)
        Nothing -> addToAL r k [v]
    where r = groupAL al

compute :: T.Text -> [(T.Text, [Float])]
compute s =
    let ls = T.lines s
        h = head ls
        vs = tail ls
        pvs = map (getField 4 &&& getFieldValue 37) vs
    in map (second catMaybes) $ groupAL pvs

main :: IO ()
main = do
    putStrLn "Hello, World!"
    args <- getArgs
    content <- C8.readFile (head args)
    print $ compute $ decodeUtf8 content

