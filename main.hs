
import Prelude
import System.Random
import Data.List

vowels = "aeiou"
consonants = "bcdfghjklmnpqrstvwxyz"

main = do
  gen <- newStdGen
  let numbers = randoms gen :: [Int]
  let nameLength = round $ 4.0 + (fromIntegral ((head numbers) `mod` 100) / 100.0) * 3.0
  let seeds = tail numbers
  let name = nameOfLength seeds nameLength
  putStrLn name

-- Chooses an element from a list given an indexing value
choose :: String -> Int -> Char
choose set x
  = set !! (x `mod` (length set))

chooseConsonant = choose consonants
chooseVowel = choose vowels

-- Generate a random name with a normally distributed length including 0
name :: [Int] -> String
name (x:y:xs) 
  | (x `mod` 7) == 0 = ""
  | otherwise = chooseConsonant x : chooseVowel y : name xs
 
-- Generate a random name with a given number of characters
nameOfLength :: [Int] -> Int -> String
nameOfLength seeds size =
  let nums = take size $ pairs $ seeds
      f    = \x -> (chooseConsonant $ fst x) : (chooseVowel $ fst x) : ""
  in foldl (++) "" $ map f nums

-- Take every n elements of a list
each :: Int -> [a] -> [a]
each n = map head . takeWhile (not . null) . iterate (drop n)

-- Pair up each 2 elements into a tuple
pairs :: [a] -> [(a, a)]
pairs xs
  = each 2 . (zip <*> drop 1) $ xs
