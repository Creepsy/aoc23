import Data.Char (digitToInt, isDigit)
import Data.List (findIndex, isPrefixOf)

type CalibrationDocument = [String]

main :: IO ()
main = do
  calibrationDocument <- lines <$> readFile "input.txt"
  
  putStrLn $ "Part 1: " ++ show (solvePart1 calibrationDocument)
  putStrLn $ "Part 2: " ++ show (solvePart2 calibrationDocument)

solvePart1 :: CalibrationDocument -> Int
solvePart1 = sum . map recoverCalibrationValue1

solvePart2 :: CalibrationDocument -> Int
solvePart2 = sum . map recoverCalibrationValue2

recoverCalibrationValue1 :: String -> Int
recoverCalibrationValue1 = read . ends . filter isDigit

recoverCalibrationValue2 :: String -> Int
recoverCalibrationValue2 noisyValue = let [first, last] = ends . extractDigits $ noisyValue in 10 * first + last

extractDigits :: String -> [Int]
extractDigits str@(c : rs)
  | isDigit c = digitToInt c : extractDigits rs
  | otherwise = case findIndex (`isPrefixOf` str) numbers of
      Just i -> i + 1 : extractDigits (tail str)
      Nothing -> extractDigits (tail str)
  where
    numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
extractDigits [] = []

ends :: [a] -> [a]
ends list = [head list, last list]