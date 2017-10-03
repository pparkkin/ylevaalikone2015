{-# LANGUAGE OverloadedStrings #-}

module DBLoad where

import Data.Text.Encoding ( decodeUtf8 )
import Data.Vector ( Vector )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Vector as V

import Database.SQLite.Simple


-- TODO: Performance
uniques :: Eq a => Vector a -> Vector a
uniques = V.fromList . (V.foldr (\r a -> if elem r a then a else r:a) [])

loadData :: Vector (Vector B.ByteString) -> Connection -> IO ()
loadData csvData conn = do
  putStrLn "Loading data into database."
  let ps = V.indexed $ uniques $ V.map (V.! 4) csvData
  loadPuolueet ps conn

loadPuolueet :: Vector (Int, B.ByteString) -> Connection -> IO ()
loadPuolueet d c = do
  putStrLn "Loading puolueet."
  V.mapM_ (\r -> execute c "INSERT INTO puolueet (id, name) VALUES (?, ?)" ((fst r :: Int), (decodeUtf8 (snd r) :: T.Text))) d
