{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import System.Exit
import System.IO
import System.Environment

import qualified Data.Text as T

import Database.SQLite.Simple

createQueries = "create_tables.sql"

createTables :: Connection -> IO ()
createTables conn = do
    sql <- readFile createQueries
    let qs = filter (not . T.null) $ map stripComments $ T.splitOn ";" (T.pack sql)
    mapM_ (execute_ conn) (map Query qs)
  where
    stripComments = T.strip . T.unlines . (map (fst . (T.breakOn "--"))) . T.lines

main :: IO ()
main = do
  args <- getArgs
  when (length args < 1) $ do
    hPutStrLn stderr "Missing database file argument."
    exitWith (ExitFailure 1)
  putStrLn "Creating tables."
  withConnection (head args) createTables
