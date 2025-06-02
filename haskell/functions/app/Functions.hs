module Main (main) where

-- number of subsets that exist without including the empty set
powerSetWithoutEmpty :: Int -> Int
powerSetWithoutEmpty listLength = 2 ^ listLength - 1

main :: IO ()
main = print $ powerSetWithoutEmpty 3
