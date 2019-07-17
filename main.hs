
import System.Random

vowels = "aeiou"
consonants = "bcdfghjklmnpqrstvwxyz"

name :: [Int] -> String
name seeds 
  | (seeds !! 0 `mod` 7) == 0 = ""
  | otherwise = (consonants !! (seeds !! 0 `mod` 21)) : ( (vowels !! (seeds !! 1 `mod` 5)) : name (drop 2 seeds))

main = do
  gen <- newStdGen
  let ns = randoms gen :: [Int]
  putStrLn  ( name ns)
