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


-- TODO: Performance
uniques :: Eq a => Vector a -> Vector a
uniques = V.fromList . (V.foldr (\r a -> if elem r a then a else r:a) [])

textColumn :: Int -> Vector (Vector B.ByteString) -> Vector T.Text
textColumn n = V.map (decodeUtf8 . (V.! n))

loadData :: Vector (Vector B.ByteString) -> Connection -> IO ()
loadData csvData conn = do
  putStrLn "Loading data into database."
  let vps = V.map parseVaalipiiriID $ uniques $ textColumn 0 csvData
  loadVaalipiirit vps conn
  let ps = V.indexed $ uniques $ textColumn 4 csvData
  loadPuolueet ps conn

loadCollectionTable :: String -> Vector (Int, T.Text) -> Connection -> IO ()
loadCollectionTable n d c = do
  putStrLn ("Loading table " ++ n ++ ".")
  V.mapM_ (\r -> execute c q (vs r)) d
  where
    q = Query $ T.pack $ "INSERT INTO " ++ n ++ " (id, name) VALUES (?, ?)"
    vs r = ((fst r :: Int), (snd r :: T.Text))

parseVaalipiiriID :: T.Text -> (Int, T.Text)
parseVaalipiiriID t = (vpID, vpName)
  where
    (vpID, vpName) =
      case TR.decimal t of
        Right (i, n) -> (i, T.strip n)

loadVaalipiirit :: Vector (Int, T.Text) -> Connection -> IO ()
loadVaalipiirit = loadCollectionTable "vaalipiirit"

loadPuolueet :: Vector (Int, T.Text) -> Connection -> IO ()
loadPuolueet = loadCollectionTable "puolueet"
