{-# LANGUAGE OverloadedStrings #-}

module Voting (main) where

import Control.Monad (replicateM)
import System.Random (randomRIO)

-- Data types equivalent to Rust structs
data Person = Person
  { personName :: String,
    personUuid :: String
  }
  deriving (Show, Eq)

data Poll = Poll
  { pollName :: String,
    pollOptions :: [Option]
  }
  deriving (Show)

data Option = Option
  { optionName :: Person,
    optionVotes :: Int
  }
  deriving (Show)

data Vote = Vote
  { votePerson :: String,
    voteOption :: String
  }
  deriving (Show)

-- Generate a random alphanumeric character
randomAlphanumeric :: IO Char
randomAlphanumeric = do
  let chars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
  idx <- randomRIO (0, length chars - 1)
  return $ chars !! idx

-- Generate a random string of specified length
randomString :: Int -> IO String
randomString charNum = replicateM charNum randomAlphanumeric

-- Generate multiple random strings
generateRandomStrings :: Int -> Int -> IO [String]
generateRandomStrings count charNum = replicateM count (randomString charNum)

-- Vote simulation function
voteSimulation :: IO ()
voteSimulation = do
  -- Create candidates
  let candidateNames = ["John", "Jane", "Jim"]
  candidates <-
    mapM
      ( \name -> do
          uuid <- randomString 10
          return $
            Option
              { optionName = Person {personName = name, personUuid = uuid},
                optionVotes = 0
              }
      )
      candidateNames

  -- Create voters
  voterNames <- generateRandomStrings 100 10
  voters <-
    mapM
      ( \name -> do
          uuid <- randomString 10
          return $ Person {personName = name, personUuid = uuid}
      )
      voterNames

  -- Each voter picks a random vote
  votes <-
    mapM
      ( \voter -> do
          candidateIdx <- randomRIO (0, length candidates - 1)
          let selectedCandidate = candidates !! candidateIdx
          return $
            Vote
              { votePerson = personUuid voter,
                voteOption = personUuid (optionName selectedCandidate)
              }
      )
      voters

  -- Print some results
  putStrLn $ "Created " ++ show (length candidates) ++ " candidates"
  putStrLn $ "Created " ++ show (length voters) ++ " voters"
  putStrLn $ "Generated " ++ show (length votes) ++ " votes"

  -- Print candidate names
  putStrLn "Candidates:"
  mapM_ (\candidate -> putStrLn $ "  " ++ personName (optionName candidate)) candidates

  -- Print first few votes as example
  putStrLn "First 5 votes:"
  mapM_ (\vote -> putStrLn $ "  Voter " ++ take 8 (votePerson vote) ++ "... voted for " ++ take 8 (voteOption vote) ++ "...") (take 5 votes)

main :: IO ()
main = do
  putStrLn "Running voting simulation..."
  voteSimulation
  putStrLn "Simulation complete!"
