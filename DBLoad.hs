{-# LANGUAGE OverloadedStrings #-}

module DBLoad where

import Data.Function ( on )
import Data.Int ( Int64 )
import Data.List ( intercalate, groupBy )
import Data.Maybe ( catMaybes )
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
-- table and pull into normalized tables from there using SQL. Problem
-- is the large number of columns from the questions.

loadData :: (Vector B.ByteString, Vector (Vector B.ByteString)) -> Connection -> IO ()
loadData (headers, csvData) conn = do
  putStrLn "Loading data into database."
  mapM_ (\(t, f) -> loadCollectionTable t (f csvData) conn) collectionTables
  let kysymykset = V.drop 39 headers
  loadKysymyksetTable conn kysymykset
  loadVastaajatTable csvData conn

loadKysymyksetTable :: Connection -> Vector B.ByteString -> IO ()
loadKysymyksetTable conn ks = do
  let cs = V.toList $ V.map (decodeUtf8) ks
      qs = map (parseKysymys . head) $ groupBy ((==) `on` (T.take 3)) cs
  putStrLn "Loading table kysymykset."
  mapM_ (loadKysymyksetRow conn) qs

loadKysymyksetRow :: Connection -> (Int, T.Text) -> IO ()
loadKysymyksetRow conn vs = do
  let q = Query $ "INSERT INTO kysymykset (id, kysymys) VALUES (?, ?)"
  execute conn q vs

loadVastaajatTable :: Vector (Vector B.ByteString) -> Connection -> IO ()
loadVastaajatTable csvData conn = do
  putStrLn "Loading table vastaajat."
  V.mapM_ (loadVastaajatRow conn) csvData
  putStrLn ""

loadVastaajatRow :: Connection -> Vector B.ByteString -> IO ()
loadVastaajatRow conn row = do
  params <- constructVastaajaParams conn row
  execute conn query params
  let (SQLInteger vid) = head params
  mapM_ (loadManyToMany conn row vid) manyToManyMapping
  putStr "."
  where
    fieldNames = map fst fieldMapping
    fields = intercalate "," fieldNames
    qms = intercalate "," $ take (length fieldNames) $ repeat "?"
    query = Query $ T.pack $ "INSERT INTO vastaajat (" ++ fields ++ ") VALUES (" ++ qms ++ ")"

loadManyToMany :: Connection -> Vector B.ByteString -> Int64 -> ManyToMany -> IO ()
loadManyToMany conn row vid (ManyToMany jt cn c vt) = do
  let vals = V.toList $ uniques $ parseMultiple $ textColumnValue c row
  ids <- queryIds conn vt vals
  mapM_ (loadManyToManyEntry conn jt cn vid) (zip vals ids)

loadManyToManyEntry :: Connection -> T.Text -> T.Text -> Int64 -> (T.Text, Maybe Int64) -> IO ()
loadManyToManyEntry conn t c vid (_, (Just cid)) = do
  let q = Query $ "INSERT INTO " `T.append` t `T.append` " (vastaaja_id, " `T.append` c `T.append` ") values (?, ?)"
      p = (vid, cid)
  execute conn q p
loadManyToManyEntry _ t _ _ (v, Nothing) = do
  putStrLn $ "Could not find ID for value " ++ (T.unpack v) ++ " for table " ++ (T.unpack t)

constructVastaajaParams :: Connection -> Vector B.ByteString -> IO [SQLData]
constructVastaajaParams conn row = mapM ((constructVastaajaParam conn row) . snd) fieldMapping

textToInt :: T.Text -> Either T.Text Int64
textToInt cv =
  case TR.decimal cv of
    Right (i, _) -> Right i
    Left l -> Left (T.pack l)

kyllaEiToInt :: T.Text -> Either T.Text Int64
kyllaEiToInt "kyllä" = Right 1
kyllaEiToInt "ei" = Right 0
kyllaEiToInt _ = Right (-1)

constructVastaajaParam :: Connection -> Vector B.ByteString -> FieldMapping -> IO SQLData
constructVastaajaParam _ row (IntColumnIndex n f) = do
  let cv = textColumnValue n row
      iv = f cv
  case iv of
    Right i ->
      return (SQLInteger i)
    Left err -> do
      putStrLn $ "Unable to parse integer value '" ++ (T.unpack cv) ++ "' from column '" ++ (show n) ++ "'"
      return (SQLInteger (-1))
constructVastaajaParam _ row (TextColumnIndex n) = do
  let cv = textColumnValue n row
  return (SQLText cv)
constructVastaajaParam c row (TableReference n t) = do
  let cv = textColumnValue n row
  rs <- queryId c t cv
  case rs of
    Just i ->
      return (SQLInteger i)
    _ -> do
      putStrLn $ "Unable to find value '" ++ (T.unpack cv) ++ "' in table '" ++ (T.unpack t) ++ "'"
      return (SQLInteger (-1))

loadCollectionTable :: String -> Vector (Int, T.Text) -> Connection -> IO ()
loadCollectionTable n d c = do
  putStrLn ("Loading table " ++ n ++ ".")
  V.mapM_ (\r -> execute c q (vs r)) d
  where
    q = Query $ T.pack $ "INSERT INTO " ++ n ++ " (id, value) VALUES (?, ?)"
    vs r = ((fst r :: Int), (snd r :: T.Text))

uniques :: Eq a => Vector a -> Vector a
uniques = V.fromList . (V.foldr (\r a -> if elem r a then a else r:a) [])

textColumn :: Int -> Vector (Vector B.ByteString) -> Vector T.Text
textColumn n = V.map (textColumnValue n)

textColumns :: [Int] -> Vector (Vector B.ByteString) -> Vector T.Text
textColumns [] = const V.empty
textColumns ns = foldr1 (concatColumns) (map textColumn ns)

multiColumn :: Int -> Vector (Vector B.ByteString) -> Vector T.Text
multiColumn n = (V.concatMap parseMultiple) . (textColumn n)

concatColumns :: (Vector (Vector B.ByteString) -> Vector T.Text)
              -> (Vector (Vector B.ByteString) -> Vector T.Text)
              -> Vector (Vector B.ByteString)
              -> Vector T.Text
concatColumns one two v = mappend (one v) (two v)

textColumnValue :: Int -> Vector B.ByteString -> T.Text
textColumnValue n vector =
  let cv = T.strip $ decodeUtf8 $ vector V.! n
  -- Use corrected value from valueMapping if available
  in case lookup cv valueMapping of
    Just v -> v
    Nothing -> cv

parseVaalipiiriID :: T.Text -> (Int, T.Text)
parseVaalipiiriID t = (vpID, vpName)
  where
    (vpID, vpName) =
      case TR.decimal t of
        Right (i, n) -> (i, T.strip n)

parseKysymys :: T.Text -> (Int, T.Text)
parseKysymys t = (i, k)
  where
    (i, k) =
      case TR.decimal t of
        Right (i, r) -> (i, T.drop 1 r)

parseMultiple :: T.Text -> Vector T.Text
parseMultiple = V.fromList . splitValues

splitValues :: T.Text -> [T.Text]
splitValues = (map T.strip) . (T.splitOn "|")

queryIds :: Connection -> T.Text -> [T.Text] -> IO [Maybe Int64]
queryIds conn t = mapM (queryId conn t)

queryId :: Connection -> T.Text -> T.Text -> IO (Maybe Int64)
queryId c t cv = do
  let q = Query $ "SELECT id FROM " `T.append` t `T.append` " WHERE value LIKE ?"
  rs <- query c q (Only cv)
  case rs of
    [[i]] ->
      return (Just i)
    _ -> do
      return Nothing

collectionTables =
  [ ("vaalipiirit", (V.map parseVaalipiiriID) . uniques . (textColumn 0))
  , ("puolueet", V.indexed . uniques . (textColumn 4))
  , ("sukupuolet", V.indexed . uniques . (textColumn 6))
  , ("kotikunnat", V.indexed . uniques . (textColumn 11))
  , ("kielet", V.indexed . uniques . (concatColumns (multiColumn 29) (textColumn 18)))
  , ("koulutukset", V.indexed . uniques . (textColumn 28))
  , ("uskonnolliset_yhteisot", V.indexed . uniques . (textColumn 30))
  , ("kokemukset", V.indexed . uniques . (multiColumn 32))
  , ("vaalibudjetit", V.indexed . uniques . (textColumn 33))
  , ("ulkopuolisen_rahoituksen_osuudet", V.indexed . uniques . (textColumn 34))
  , ("ulkopuolisen_rahoituksen_lahteet", V.indexed . uniques . (textColumn 35))
  , ("vuositulot", V.indexed . uniques . (textColumn 37))
  , ("sijoitukset", V.indexed . uniques . (textColumn 38))
  , ("vastaukset", V.indexed . uniques . (textColumns vastausColumns))
  ]

vastausColumns :: [Int]
vastausColumns = [39,41..257]

-- Used to fix some spelling mistakes
valueMapping :: [(T.Text, T.Text)]
valueMapping = [ ("lappeenranta", "Lappeenranta") ]

data FieldMapping = IntColumnIndex Int (T.Text -> Either T.Text Int64)-- field value is an int value from a column, includes conversion function
                  | TextColumnIndex Int -- field value is a text value from a column
                  | TableReference Int T.Text -- field value is a reference to another table

-- Map fields in `vastaajat` table to CSV columns and other tables
fieldMapping :: [(String, FieldMapping)]
fieldMapping = [ ("id", IntColumnIndex 1 textToInt)
               , ("sukunimi", TextColumnIndex 2)
               , ("etunimi", TextColumnIndex 3)
               , ("puolue", TableReference 4 "puolueet")
               , ("ika", IntColumnIndex 5 textToInt)
               , ("sukupuoli", TableReference 6 "sukupuolet")
               , ("kansanedustaja", IntColumnIndex 7 textToInt)
               , ("vastattu", TextColumnIndex 8)
               , ("valittu", IntColumnIndex 9 textToInt)
               , ("sitoutumaton", IntColumnIndex 10 textToInt)
               , ("kotikunta", TableReference 11 "kotikunnat")
               , ("ehdokasnumero", IntColumnIndex 12 textToInt)
               , ("miksi_eduskuntaan", TextColumnIndex 13)
               , ("mita_edistaa", TextColumnIndex 14)
               , ("vaalilupaus1", TextColumnIndex 15)
               , ("vaalilupaus2", TextColumnIndex 16)
               , ("vaalilupaus3", TextColumnIndex 17)
               , ("aidinkieli", TableReference 18 "kielet")
               , ("kotisivu", TextColumnIndex 19)
               , ("facebook", TextColumnIndex 20)
               , ("twitter", TextColumnIndex 21)
               , ("lapsia", IntColumnIndex 22 kyllaEiToInt)
               , ("perhe", TextColumnIndex 23)
               , ("vapaa_ajalla", TextColumnIndex 24)
               , ("tyonantaja", TextColumnIndex 25)
               , ("ammattiasema", TextColumnIndex 26)
               , ("ammatti", TextColumnIndex 27)
               , ("koulutus", TableReference 28 "koulutukset")
               , ("uskonnollinen_yhteiso", TableReference 30 "uskonnolliset_yhteisot")
               , ("puolueen_jasen", IntColumnIndex 31 kyllaEiToInt)
               , ("vaalibudjetti", TableReference 33 "vaalibudjetit")
               , ("ulkopuolisen_rahoituksen_osuus", TableReference 34 "ulkopuolisen_rahoituksen_osuudet")
               , ("ulkopuolisen_rahoituken_lahde", TableReference 35 "ulkopuolisen_rahoituksen_lahteet")
               , ("sidonnaisuudet", TextColumnIndex 36)
               , ("vuositulot", TableReference 37 "vuositulot")
               , ("sijoitukset", TableReference 38 "sijoitukset")
               ]

data ManyToMany = ManyToMany T.Text T.Text Int T.Text
                -- "join_table_name" "join_table_column_name" <csv_column> "value_table"

manyToManyMapping :: [ManyToMany]
manyToManyMapping = [ ManyToMany "kielitaidot" "kieli_id" 29 "kielet"
                    , ManyToMany "poliittiset_kokemukset" "kokemus_id" 32 "kokemukset"
                    ]
