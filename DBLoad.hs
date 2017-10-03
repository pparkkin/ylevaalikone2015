{-# LANGUAGE OverloadedStrings #-}

module DBLoad where

import Data.Text.Encoding ( decodeUtf8 )
import Data.Vector ( Vector )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Vector as V

import Database.SQLite.Simple

-- NOTE
-- I'm going directly from the CSV data to normalized tables. Possibly
-- a better option might be to dump the CSV into a single raw data
-- table and pull into normalized tables from there using SQL.

loadData :: Vector (Vector B.ByteString) -> Connection -> IO ()
loadData csvData conn = do
  putStrLn "Loading data into database."
  mapM_ (\(t, f) -> loadCollectionTable t (f csvData) conn) collectionTables

collectionTables =
  [ ("vaalipiirit", (V.map parseVaalipiiriID) . uniques . (textColumn 0))
  , ("puolueet", V.indexed . uniques . (textColumn 4))
  , ("sukupuolet", V.indexed . uniques . (textColumn 6))
  ]

loadCollectionTable :: String -> Vector (Int, T.Text) -> Connection -> IO ()
loadCollectionTable n d c = do
  putStrLn ("Loading table " ++ n ++ ".")
  V.mapM_ (\r -> execute c q (vs r)) d
  where
    q = Query $ T.pack $ "INSERT INTO " ++ n ++ " (id, name) VALUES (?, ?)"
    vs r = ((fst r :: Int), (snd r :: T.Text))

-- TODO: Performance
uniques :: Eq a => Vector a -> Vector a
uniques = V.fromList . (V.foldr (\r a -> if elem r a then a else r:a) [])

textColumn :: Int -> Vector (Vector B.ByteString) -> Vector T.Text
textColumn n = V.map (decodeUtf8 . (V.! n))

parseVaalipiiriID :: T.Text -> (Int, T.Text)
parseVaalipiiriID t = (vpID, vpName)
  where
    (vpID, vpName) =
      case TR.decimal t of
        Right (i, n) -> (i, T.strip n)
