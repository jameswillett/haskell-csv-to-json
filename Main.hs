module Main where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import qualified Data.Text as T
import Control.Applicative ((<|>))
import qualified Data.List as L
import System.Environment (getArgs)

data CsvValue =
  CsvString String
  | CsvInteger Integer
  | CsvFloat Float
  | CsvNull
  deriving (Show, Eq)

readV v = fromMaybe CsvNull
  $   (CsvInteger <$> (readMaybe v :: Maybe Integer))
  <|> (CsvFloat   <$> (readMaybe v :: Maybe Float  ))
  <|> (CsvString  <$> Just v                        )

printV :: CsvValue -> String
printV (CsvNull)      = ""
printV (CsvString s)  = show s
printV (CsvFloat n)   = show n
printV (CsvInteger n) = show n

splitRow :: T.Text -> [T.Text]
splitRow = T.splitOn (T.pack ",")

splitRawCsv :: String -> [T.Text]
splitRawCsv = T.splitOn (T.pack "\r\n") . T.pack

parseRow :: T.Text -> [CsvValue]
parseRow = map (\v -> readV $ T.unpack v) . splitRow

makePairs header rows = map (zip header)
  $ filter (\r -> (length r) == (length header)) rows

makeJson :: [[(T.Text, CsvValue)]] -> String
makeJson csv = "[" ++ (L.intercalate "," $ map printRow csv) ++ "]"
  where printRow row = "{" ++ (L.intercalate "," $ map printPair row) ++ "}"
        printPair (k, v) = "\"" ++ T.unpack k ++ "\": " ++ printV v


doIt csv = makeJson $ makePairs (splitRow rawHeader) (map parseRow rawRows)
  where (rawHeader:rawRows) = splitRawCsv csv

run :: String -> String -> IO ()
run input output = do
  csv                 <- readFile input
  writeFile output $ doIt csv

main :: IO ()
main = do
  [input, output] <- getArgs
  run input output
