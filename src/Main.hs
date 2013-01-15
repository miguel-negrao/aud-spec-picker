module Main where

import Text.SSV
import Data.List
import Data.Ord
import System.Console.CmdArgs

absRat::Double -> Double
absRat x = if x < 1 then 1/x else x

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
parseFromAudacity string =  map (map read) x
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

processFile :: Double -> Int -> String -> String -> IO ()
processFile delta numFreqs inputPath outputPath = do
        file <- readFile (inputPath)
        let result = toCSV $ sort $ take numFreqs $ ((getFreqs delta) . parseFromAudacity) file 
        writeFile outputPath result
        --putStr result 
        
data Args = Args { delta::Double, numFreqs::Int, inPath::FilePath, outPath::FilePath } deriving (Show, Data, Typeable)

exampleArgs :: Args
exampleArgs = Args { 
        delta = 1.2 &= help "exclude frequencies near selected ones in range of delta",
        numFreqs = 18 &= help "number of frequencies",
        inPath = def &= argPos 0 &= typFile,
        outPath = def &= argPos 1 &= typFile
        } &= summary "Parse audacity spectrum analysis files"        
        
main::IO()
main = do
        args <- cmdArgs exampleArgs 
        processFile (delta args) (numFreqs args) (inPath args) (outPath args)