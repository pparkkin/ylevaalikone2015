{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Network.Download
import System.Exit
import System.IO
import System.Environment

import qualified Data.ByteString as B
import qualified Data.Text as T

import Database.SQLite.Simple

createQueries = "create_tables.sql"
dataURL = "http://data.yle.fi/dokumentit/Eduskuntavaalit2015/vastaukset_avoimena_datana.csv"

createTables :: Connection -> IO ()
createTables conn = do
    sql <- readFile createQueries
    let qs = filter (not . T.null) $ map stripComments $ T.splitOn ";" (T.pack sql)
    mapM_ (execute_ conn) (map Query qs)
  where
    stripComments = T.strip . T.unlines . (map (fst . (T.breakOn "--"))) . T.lines

loadData :: B.ByteString -> IO ()
loadData csvData = do
  putStrLn "Loading data into database."

main :: IO ()
main = do
  args <- getArgs
  when (length args < 1) $ do
    hPutStrLn stderr "Missing database file argument."
    exitWith (ExitFailure 1)
  putStrLn "Creating tables."
  withConnection (head args) createTables
  putStrLn "Downloading data file."
  csvRsp <- openURI dataURL
  case csvRsp of
    Left err -> do
      hPutStrLn stderr "Error downloading data file."
      hPutStrLn stderr err
      exitWith (ExitFailure 2)
    Right csvData -> do
      loadData csvData
