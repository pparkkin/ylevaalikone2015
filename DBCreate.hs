{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T

import Database.SQLite.Simple

createTables :: Connection -> IO ()
createTables conn = do
    sql <- readFile "create_tables.sql"
    let qs = filter (not . T.null) $ map stripComments $ T.splitOn ";" (T.pack sql)
    mapM_ (execute_ conn) (map Query qs)
  where
    stripComments = T.strip . T.unlines . (map (fst . (T.breakOn "--"))) . T.lines

main :: IO ()
main = do
  putStrLn "Hello, Woeld!"
  withConnection "test.db" createTables
