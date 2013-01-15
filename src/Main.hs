module Main where

import Text.SSV
import Data.List
import Data.Ord
import System.Environment

parseNum::String -> Double
parseNum numString = read numString

absRat::Double -> Double
absRat x = if x < 1 then 1/x else x

prefix:: String
prefix = "/Volumes/Data/tracks/Matilde Florest Drone/10 min";

paths :: [String]
paths = [
        prefix++"/10_annadale.txt",
        prefix++"/10_lower_crescent.txt",
        "/Volumes/Data/tracks/Matilde Florest Drone/CAIXAS 041212/DR-100_0461.txt",
        "/Volumes/Data/tracks/Matilde Florest Drone/CAIXAS 041212/DR-100_0462.txt",
        prefix++"/10_sunnyside.txt",
        prefix++"/10_lockview.txt",
        prefix++"/10_stranmillis.txt"
        ]

tabFormat :: SSVFormat
tabFormat = SSVFormat {
  ssvFormatName = "CSV",
  ssvFormatTerminator = '\r',
  ssvFormatSeparator = '\t',
  ssvFormatEscape = Nothing,
  ssvFormatStripWhite = True,
  ssvFormatQuote = Just $ SSVFormatQuote {
    ssvFormatQuoteEscape = Just '"',
    ssvFormatQuoteLeft = '"',
    ssvFormatQuoteRight = '"' } }
    
parseFromAudacity::String -> [[Double]]
parseFromAudacity string =  map (map parseNum) x
        where _:x = readSSV tabFormat string
        
getFreqs:: Double -> [[Double]] -> [Double]
getFreqs delta xs = similarRemoved
        where 
                sorted = reverse $ sortBy (comparing (\(_:b:_) -> b) ) xs
                justFreqs = (transpose sorted) !! 0
                similarRemoved = foldl f [] justFreqs
                f [] x = [x]
                f st x = st ++ (if notNearOtherFreqs then [x] else []) 
                        where notNearOtherFreqs = foldl (&&) True $ map (\y -> absRat (y/x) > delta ) st          

toCSV::[Double] -> String
toCSV xs = foldl (++) "" $ map (\x -> (show x) ++ "\n" ) xs

processFile :: Double -> Int -> String -> IO ()
processFile delta numFreqs path = do
        file <- readFile (path)
        let result = toCSV $ sort $ take numFreqs $ ((getFreqs delta) . parseFromAudacity) file
        let writePath = (reverse $ drop 4 (reverse path)) ++ "-selected.txt"   
        writeFile writePath result
        putStr result 
        
main::IO()
main = sequence_ $ map (processFile 1.2 18) paths
--main = processFile 1.2 18 (paths !! 0)
--main = main2

main2 = do
        delta:numFreqs:path:_ <- getArgs
        processFile (parseNum delta) (read numFreqs) path
        
main3 = do
        delta:numFreqs:_ <- getArgs
        sequence_ $ map (processFile (read delta) (read numFreqs) ) paths