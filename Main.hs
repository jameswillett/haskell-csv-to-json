module Main where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.Text (Text, splitOn, pack, unpack)
import Control.Applicative ((<|>))
import Data.List (intercalate)
import System.Environment (getArgs)

data CsvValue =
  CsvString String
  | CsvInteger Integer
  | CsvFloat Float
  | CsvNull
  deriving (Show, Eq)

readV :: String -> CsvValue
readV v = fromMaybe CsvNull
  $   (CsvInteger <$> (readMaybe v :: Maybe Integer))
  <|> (CsvFloat   <$> (readMaybe v :: Maybe Float  ))
  <|> (CsvString  <$> Just v                        )

printV :: CsvValue -> String
printV (CsvNull)      = ""
printV (CsvString s)  = show s
printV (CsvFloat n)   = show n
printV (CsvInteger n) = show n

splitRow :: Text -> [Text]
splitRow = splitOn (pack ",")

splitRawCsv :: String -> [Text]
splitRawCsv = splitOn (pack "\r\n") . pack

parseRow :: Text -> [CsvValue]
parseRow = map (readV . unpack) . splitRow

makePairs :: [Text] -> [[CsvValue]] -> [[(Text, CsvValue)]]
makePairs header rows = map (zip header)
  $ filter (((== length header)) . length) rows

makeJson :: [[(Text, CsvValue)]] -> String
makeJson csv = "[" ++ (intercalate "," $ map printRow csv) ++ "]"
  where printRow row     = "{" ++ (intercalate "," $ map printPair row) ++ "}"
        printPair (k, v) = "\"" ++ unpack k ++ "\": " ++ printV v

parseCsv :: String -> String
parseCsv csv = makeJson $ makePairs header rows
  where (rawHeader:rawRows) = splitRawCsv csv
        header              = splitRow rawHeader
        rows                = map parseRow rawRows

run :: String -> String -> IO ()
run input output = do
  csv <- readFile input
  writeFile output $ parseCsv csv

main :: IO ()
main = do
  (input:output:_) <- getArgs
  run input output
