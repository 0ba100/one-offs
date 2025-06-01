{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM, forM_, replicateM)
import Data.List (find)
import System.Random (randomRIO)

newtype UUID = UUID String deriving (Show, Eq)

makeUUID :: IO UUID
makeUUID = UUID <$> randomString 10

data Person = Person {personName :: String, personUuid :: UUID} deriving (Show, Eq)

data Poll = Poll {pollName :: String, pollOptions :: [Option]} deriving (Show)

data Option = Option {optionPerson :: UUID, optionVotes :: Int} deriving (Show)

data Vote = Vote {votePerson :: UUID, voteOption :: UUID} deriving (Show)

randomAlphanumeric :: IO Char
randomAlphanumeric = (charSelection !!) <$> randomRIO (0, length charSelection - 1)
  where
    charSelection = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

randomString :: Int -> IO String
randomString n = replicateM n randomAlphanumeric

prettyPrintResults :: [Option] -> [Person] -> IO ()
prettyPrintResults options runners =
  forM_ options $ \option -> case find ((== optionPerson option) . personUuid) runners of
    Just person -> putStrLn $ "  " ++ personName person ++ ": " ++ show (optionVotes option)
    Nothing -> putStrLn $ "  Unknown candidate: " ++ show (optionPerson option)

voteSimulation :: IO ([Option], [Person])
voteSimulation = do
  runners <- forM ["John", "Jane", "Jim"] $ \name -> Person name <$> makeUUID

  voters <- replicateM 100000 $ Person <$> randomString 10 <*> makeUUID

  votes <- forM voters $ \voter -> do
    selected <- (runners !!) <$> randomRIO (0, length runners - 1)
    pure $ Vote (personUuid voter) (personUuid selected)

  let countVotesFor uuid = length $ filter (== uuid) (map voteOption votes)
  let candidates = map (\r -> Option (personUuid r) (countVotesFor (personUuid r))) runners

  return (candidates, runners)

main :: IO ()
main = do
  (results, runners) <- voteSimulation
  prettyPrintResults results runners
