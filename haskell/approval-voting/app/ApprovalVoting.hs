module Main (main) where

import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUIDv4
import System.Random (randomRIO, getStdGen, Random (randomR), getStdRandom)
import Control.Monad (replicateM)
import Data.List (nub, findIndex, elemIndex)
import Data.Traversable (for, traverse)
import GHC.Base (maxInt)
import Safe (atMay)

newtype Ballot = Ballot { ballotVotes :: [UUID] } deriving (Show)

data Candidate = Candidate { candidateName :: String, candidateUUID :: UUID } deriving (Show, Eq)

newCandidate :: String -> IO Candidate
newCandidate name = Candidate name <$> UUIDv4.nextRandom

-- get an arbitrary number of elements from a set without repetition
randomSubset :: [a] -> IO [a]
randomSubset xs = do
    indices <- randomRIO (0, length xs) >>= flip randomBoundedSetIO (length xs)
    return $ map (xs !!) indices

randomBoundedSetIO :: Int -> Int -> IO [Int]
randomBoundedSetIO len upper = do
    -- this doesn't work because the set needs to get passed around
    randomNumber <- getRandomNumberNotInList [] upper 
    randomNumber : (randomBoundedSetIO (len - 1) upper)

getRandomNumberNotInList :: [Int] -> Int -> IO (Maybe Int)
getRandomNumberNotInList list upper
    | setExhaustsIndexes list = return Nothing
    | otherwise = randomExcInt list upper
  where 
    randomExcInt li u = do
        n <- randomRIO (0, u)
        if inList li n then randomExcInt li u else return (Just n)

inList :: Eq a => [a] -> a -> Bool
inList list number = case elemIndex number list of
    Just _ -> True
    Nothing -> False


setExhaustsIndexes :: [Int] -> Bool
setExhaustsIndexes list = (maximum list - 1) >= length list

-- verify that the list has not exhaustively covered all options
-- Generate a random number
-- check if it's in the list
-- if it is, generate another.
-- if not, return it.

main :: IO ()
main = do
    candidates <- mapM newCandidate ["John", "Jane", "Jim"]
    
    ballots <- replicateM 1000 $ do
        numVotes <- randomRIO (1,3)
        selectedCandidates <- randomSubset candidates numVotes
        return $ Ballot (map candidateUUID selectedCandidates)
    
    print $ take 5 ballots
