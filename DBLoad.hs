{-# LANGUAGE OverloadedStrings #-}

module DBLoad where

import Control.Monad.Trans.State ( StateT, evalStateT )
import Control.Monad.Trans ( lift )
import Data.Function ( on )
import Data.List ( intercalate, groupBy )
import Data.Map.Strict ( Map )
import Data.Maybe ( catMaybes )
import Data.Text.Encoding ( decodeUtf8 )
import Data.Tuple ( swap )
import Data.Vector ( Vector )

import qualified Control.Monad.Trans.State as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Text.Read as TR
import qualified Data.Vector as V

import Database.SQLite.Simple

-- NOTE
-- I'm going directly from the CSV data to normalized tables. Possibly
-- a better option might be to dump the CSV into a single raw data
-- table and pull into normalized tables from there using SQL. Problem
-- is the large number of columns from the questions.

type IdCache = Map T.Text Int
type IdCaches = Map T.Text IdCache

cacheId :: T.Text -> (T.Text, Int) -> IdCaches -> IdCaches
cacheId t (v, i) = M.alter alterTable t
  where
    alterTable (Just m) = Just $ M.alter alterId v m
    alterTable Nothing = Just $ M.singleton v i
    alterId _ = Just i

loadData :: (Vector B.ByteString, Vector (Vector B.ByteString)) -> Connection -> IO ()
loadData durtur conn = do
  evalStateT (loadData' durtur conn) M.empty

loadData' :: (Vector B.ByteString, Vector (Vector B.ByteString)) -> Connection -> StateT IdCaches IO ()
loadData' (headers, csvData) conn = do
  lift $ putStrLn "Loading data into database."
  mapM_ (\(t, f) -> loadCollectionTable conn t (f csvData)) collectionTables
  ks <- loadKysymyksetTable conn headers
  loadVastaajatTable conn ks csvData
  lift $ putStrLn "All done."

loadKysymyksetTable :: Connection -> Vector B.ByteString -> StateT IdCaches IO [(Int, (Int, T.Text))]
loadKysymyksetTable conn headers = do
  lift $ putStrLn "Loading table kysymykset."
  lift $ executeMany conn q vs
  return ks
  where
    ks = parseKysymykset headers
    vs = map (\(i, (_, k)) -> (i, k)) ks
    q = "INSERT INTO kysymykset (id, kysymys) VALUES (?, ?)"

loadVastaajatTable :: Connection -> [(Int, (Int, T.Text))] -> Vector (Vector B.ByteString) -> StateT IdCaches IO ()
loadVastaajatTable conn ks csvData = do
  lift $ putStrLn "Loading table vastaajat."
  V.mapM_ (loadVastaajatRow conn ks) csvData
  lift $ putStrLn ""

loadVastaajatRow :: Connection -> [(Int, (Int, T.Text))] -> Vector B.ByteString -> StateT IdCaches IO ()
loadVastaajatRow conn ks row = do
  params <- sequence $ constructVastaajaParams conn row fieldMapping
  let fns = map fst params
      ps = map snd params
      fs = intercalate "," fns
      qms = intercalate "," $ take (length fns) $ repeat "?"
      q = Query $ T.pack $ "INSERT INTO vastaajat (" ++ fs ++ ") VALUES (" ++ qms ++ ")"
  lift $ execute conn q ps
  let ("id", (SQLInteger vid')) = head params
      vid = fromIntegral vid'
  mapM_ (loadManyToMany conn row vid) manyToManyMapping
  loadVastaajatVastaukset conn ks vid row
  lift $ putStr "."

loadVastaajatVastaukset :: Connection -> [(Int, (Int, T.Text))] -> Int -> Vector B.ByteString -> StateT IdCaches IO ()
loadVastaajatVastaukset conn ks vid row =
  mapM_ (loadVastaajatVastaus conn vid row) ks

loadVastaajatVastaus :: Connection -> Int -> Vector B.ByteString -> (Int, (Int, T.Text)) -> StateT IdCaches IO ()
loadVastaajatVastaus conn vid row (ki, (kc, _)) = do
  let v = textColumnValue kc row
      k = textColumnValue (kc + 1) row
  vi <- queryId conn "vastaukset" v
  case vi of
    Just i ->
      lift $ execute conn q (vid, ki, i)
    Nothing ->
      lift $ putStrLn $ "Could not find value " ++ (T.unpack v) ++ " in table vastaukset"
  where
    q = Query $ "INSERT INTO vastaaja_vastaukset (vastaaja_id, kysymys_id, vastaus_id) VALUES (?, ?, ?)"

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [x] = []
pairs (x:y:xs) = (x, y) : pairs xs

loadManyToMany :: Connection -> Vector B.ByteString -> Int -> ManyToMany -> StateT IdCaches IO ()
loadManyToMany conn row vid (ManyToMany jt cn c vt) = do
  let vals = V.toList $ uniques $ parseMultiple $ textColumnValue c row
  ids <- queryIds conn vt vals
  lift $ mapM_ (loadManyToManyEntry conn jt cn vid) (zip vals ids)

loadManyToManyEntry :: Connection -> T.Text -> T.Text -> Int -> (T.Text, Maybe Int) -> IO ()
loadManyToManyEntry conn t c vid (_, (Just cid)) = do
  let q = Query $ "INSERT INTO " `T.append` t `T.append` " (vastaaja_id, " `T.append` c `T.append` ") values (?, ?)"
      p = (vid, cid)
  execute conn q p
loadManyToManyEntry _ t _ _ (v, Nothing) = do
  putStrLn $ "Could not find ID for value " ++ (T.unpack v) ++ " for table " ++ (T.unpack t)

constructVastaajaParams :: Connection -> Vector B.ByteString -> [Maybe (String, FieldMapping)] -> [StateT IdCaches IO (String, SQLData)]
constructVastaajaParams conn row fms = foldr f [] (zip (V.toList row) fms)
  where
    f (_, Nothing) ps = ps
    f (b, Just fm) ps = fmap (\d -> (fst fm, d)) (constructVastaajaParam conn b (snd fm)) : ps

constructVastaajaParam :: Connection -> B.ByteString -> FieldMapping -> StateT IdCaches IO SQLData
constructVastaajaParam _ b (IntColumnIndex n f) = do
  let cv = textValue b
      iv = f cv
  case iv of
    Right i ->
      return (SQLInteger (fromIntegral i))
    Left err -> do
      lift $ putStrLn $ "Unable to parse integer value '" ++ (T.unpack cv) ++ "' from column '" ++ (show n) ++ "'"
      return (SQLInteger (-1))
constructVastaajaParam _ b (TextColumnIndex n) = do
  let cv = textValue b
  return (SQLText cv)
constructVastaajaParam c b (TableReference n t) = do
  let cv = textValue b
  rs <- queryId c t cv
  case rs of
    Just i ->
      return (SQLInteger (fromIntegral i))
    _ -> do
      lift $ putStrLn $ "Unable to find value '" ++ (T.unpack cv) ++ "' in table '" ++ (T.unpack t) ++ "'"
      return (SQLInteger (-1))

loadCollectionTable :: Connection -> String -> Vector (Int, T.Text) -> StateT IdCaches IO ()
loadCollectionTable c n d = do
  lift $ putStrLn ("Loading table " ++ n ++ ".")
  lift $ executeMany c q vs
  s <- S.get
  S.put $ foldr (\v m -> cacheId (T.pack n) (swap v) m) s vs
  where
    q = Query $ T.pack $ "INSERT INTO " ++ n ++ " (id, value) VALUES (?, ?)"
    vs = V.toList d

textToInt :: T.Text -> Either T.Text Int
textToInt cv =
  case TR.decimal cv of
    Right (i, _) -> Right i
    Left l -> Left (T.pack l)

kyllaEiToInt :: T.Text -> Either T.Text Int
kyllaEiToInt "kyllä" = Right 1
kyllaEiToInt "ei" = Right 0
kyllaEiToInt _ = Right (-1)

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
textColumnValue n vector = textValue (vector V.! n)

textValue :: B.ByteString -> T.Text
textValue b =
  let cv = T.strip $ decodeUtf8 b
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

parseKysymys :: T.Text -> Either String (Int, T.Text)
parseKysymys t =
  case TR.decimal t of
    Right (i, r) -> Right (i, T.drop 1 r)
    Left e -> Left e

--                 headers             ->   id  column kysymys
parseKysymykset :: Vector B.ByteString -> [(Int, (Int, T.Text))]
parseKysymykset hs = V.ifoldl selectKysymys [] hs
  where
    selectKysymys :: [(Int, (Int, T.Text))] -> Int -> B.ByteString -> [(Int, (Int, T.Text))]
    selectKysymys ks c b =
      case parseKysymys (decodeUtf8 b) of
        Right (i, k) -> insertKysymys ks i c k
        Left _ -> ks
    insertKysymys :: [(Int, (Int, T.Text))] -> Int -> Int -> T.Text -> [(Int, (Int, T.Text))]
    insertKysymys ks i c k =
      case lookup i ks of
        Just _ -> ks
        Nothing -> (i, (c, k)) : ks

parseMultiple :: T.Text -> Vector T.Text
parseMultiple = V.fromList . splitValues

splitValues :: T.Text -> [T.Text]
splitValues = (map T.strip) . (T.splitOn "|")

queryIds :: Connection -> T.Text -> [T.Text] -> StateT IdCaches IO [Maybe Int]
queryIds conn t = mapM (queryId conn t)

queryId :: Connection -> T.Text -> T.Text -> StateT IdCaches IO (Maybe Int)
queryId c t cv = do
  c <- S.get
  case M.lookup t c of
    Just m ->
      return (M.lookup cv m)
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

data FieldMapping = IntColumnIndex Int (T.Text -> Either T.Text Int)-- field value is an int value from a column, includes conversion function
                  | TextColumnIndex Int -- field value is a text value from a column
                  | TableReference Int T.Text -- field value is a reference to another table

-- Map fields in `vastaajat` table to CSV columns and other tables
fieldMapping :: [Maybe (String, FieldMapping)]
fieldMapping = [ Nothing
               , Just ("id", IntColumnIndex 1 textToInt)
               , Just ("sukunimi", TextColumnIndex 2)
               , Just ("etunimi", TextColumnIndex 3)
               , Just ("puolue", TableReference 4 "puolueet")
               , Just ("ika", IntColumnIndex 5 textToInt)
               , Just ("sukupuoli", TableReference 6 "sukupuolet")
               , Just ("kansanedustaja", IntColumnIndex 7 textToInt)
               , Just ("vastattu", TextColumnIndex 8)
               , Just ("valittu", IntColumnIndex 9 textToInt)
               , Just ("sitoutumaton", IntColumnIndex 10 textToInt)
               , Just ("kotikunta", TableReference 11 "kotikunnat")
               , Just ("ehdokasnumero", IntColumnIndex 12 textToInt)
               , Just ("miksi_eduskuntaan", TextColumnIndex 13)
               , Just ("mita_edistaa", TextColumnIndex 14)
               , Just ("vaalilupaus1", TextColumnIndex 15)
               , Just ("vaalilupaus2", TextColumnIndex 16)
               , Just ("vaalilupaus3", TextColumnIndex 17)
               , Just ("aidinkieli", TableReference 18 "kielet")
               , Just ("kotisivu", TextColumnIndex 19)
               , Just ("facebook", TextColumnIndex 20)
               , Just ("twitter", TextColumnIndex 21)
               , Just ("lapsia", IntColumnIndex 22 kyllaEiToInt)
               , Just ("perhe", TextColumnIndex 23)
               , Just ("vapaa_ajalla", TextColumnIndex 24)
               , Just ("tyonantaja", TextColumnIndex 25)
               , Just ("ammattiasema", TextColumnIndex 26)
               , Just ("ammatti", TextColumnIndex 27)
               , Just ("koulutus", TableReference 28 "koulutukset")
               , Nothing -- many_to_many
               , Just ("uskonnollinen_yhteiso", TableReference 30 "uskonnolliset_yhteisot")
               , Just ("puolueen_jasen", IntColumnIndex 31 kyllaEiToInt)
               , Nothing -- many_to_many
               , Just ("vaalibudjetti", TableReference 33 "vaalibudjetit")
               , Just ("ulkopuolisen_rahoituksen_osuus", TableReference 34 "ulkopuolisen_rahoituksen_osuudet")
               , Just ("ulkopuolisen_rahoituken_lahde", TableReference 35 "ulkopuolisen_rahoituksen_lahteet")
               , Just ("sidonnaisuudet", TextColumnIndex 36)
               , Just ("vuositulot", TableReference 37 "vuositulot")
               , Just ("sijoitukset", TableReference 38 "sijoitukset")
               ] ++ repeat Nothing

data ManyToMany = ManyToMany T.Text T.Text Int T.Text
                -- "join_table_name" "join_table_column_name" <csv_column> "value_table"

manyToManyMapping :: [ManyToMany]
manyToManyMapping = [ ManyToMany "kielitaidot" "kieli_id" 29 "kielet"
                    , ManyToMany "poliittiset_kokemukset" "kokemus_id" 32 "kokemukset"
                    ]
