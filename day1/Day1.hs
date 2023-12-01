import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)

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
recoverCalibrationValue2 noisyValue = let [first, last] = ends . extractAlphaNumDigits $ noisyValue in 10 * first + last

extractAlphaNumDigits :: String -> [Int]
extractAlphaNumDigits str
  | "one" `isPrefixOf` str = 1 : extractAlphaNumDigits (drop 2 str) -- we need to keep the e for "eight"
  | "two" `isPrefixOf` str = 2 : extractAlphaNumDigits (drop 2 str) -- we need to keep the o for "one"
  | "three" `isPrefixOf` str = 3 : extractAlphaNumDigits (drop 4 str) -- we need to keep the e for "eight"
  | "four" `isPrefixOf` str = 4 : extractAlphaNumDigits (drop 4 str)
  | "five" `isPrefixOf` str = 5 : extractAlphaNumDigits (drop 3 str) -- we need to keep the e for "eight"
  | "six" `isPrefixOf` str = 6 : extractAlphaNumDigits (drop 3 str)
  | "seven" `isPrefixOf` str = 7 : extractAlphaNumDigits (drop 4 str) -- we need to keep the n for "nine"
  | "eight" `isPrefixOf` str = 8 : extractAlphaNumDigits (drop 4 str)
  | "nine" `isPrefixOf` str = 9 : extractAlphaNumDigits (drop 3 str) -- we need to keep the e for "eight"
extractAlphaNumDigits (c : rs)
  | isDigit c = digitToInt c : extractAlphaNumDigits rs
  | otherwise = extractAlphaNumDigits rs
extractAlphaNumDigits [] = []

ends :: [a] -> [a]
ends list = [head list, last list]