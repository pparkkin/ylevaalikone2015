{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception.Base ( SomeException
                              , tryJust
                              )
import Control.Monad
import Data.Char ( ord )
import Data.Csv
import Data.Vector ( Vector )
import Network.Download
import System.Directory ( doesFileExist )
import System.Environment
import System.Exit
import System.IO

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Vector as V

import Database.SQLite.Simple

import DBLoad

createQueries = "create_tables.sql"
dataFileName = "vastaukset_avoimena_datana.csv"
dataURL = "http://data.yle.fi/dokumentit/Eduskuntavaalit2015/vastaukset_avoimena_datana.csv"
decodeOptions = defaultDecodeOptions {
      decDelimiter = fromIntegral (ord ';')
    }

createTables :: Connection -> IO ()
createTables conn = do
    sql <- readFile createQueries
    let qs = filter (not . T.null) $ map stripComments $ T.splitOn ";" (T.pack sql)
    mapM_ (execute_ conn) (map Query qs)
  where
    stripComments = T.strip . T.unlines . (map (fst . (T.breakOn "--"))) . T.lines

readData :: IO (Either String B.ByteString)
readData = do
  putStrLn ("Reading data from file (" ++ dataFileName ++ ")")
  tryJust (\(e :: SomeException) -> Just $ show e) (B.readFile dataFileName)

downloadData :: IO (Either String B.ByteString)
downloadData = do
  putStrLn "Downloading data file."
  openURI dataURL

decodeCsv :: B.ByteString -> IO (Either String (Vector B.ByteString, Vector (Vector B.ByteString)))
decodeCsv csvData = do
  putStrLn "Decoding CVS data."
  let r = decodeWith decodeOptions NoHeader (BL.fromStrict csvData)
  case r of
    Left err -> return $ Left err
    Right rs -> return $ Right (V.head rs, V.tail rs)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  when (length args < 1) $ do
    hPutStrLn stderr "Missing database file argument."
    exitWith (ExitFailure 1)
  putStrLn "Creating tables."
  withConnection (head args) createTables
  e <- doesFileExist dataFileName
  csvData <-
    if e
      then readData
      else downloadData
  vectorData <-
    case csvData of
      Left err -> do
        hPutStrLn stderr "Error getting data."
        hPutStrLn stderr err
        exitWith (ExitFailure 2)
      Right c -> do
        decodeCsv c
  case vectorData of
    Left err -> do
      hPutStrLn stderr "Error decoding data."
      hPutStrLn stderr err
      exitWith (ExitFailure 3)
    Right v -> do
      withConnection (head args) (loadData v)
