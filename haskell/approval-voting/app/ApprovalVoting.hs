module Main (main) where

import Data.Map (Map, fromList)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUIDv4

data BallotBox = BallotBox {
    ballotBoxCandidates :: [Candidate],
    ballotBoxVotes :: Map UUID Int
} deriving (Show)

-- The functional way to do this probably avoids creating a ballot box until all data is available.

newBallotBox :: [Candidate] -> BallotBox
newBallotBox candidates = BallotBox candidates $ fromList $ map (\c -> (candidateUUID c, 0)) candidates

ballotBoxVote :: BallotBox -> [UUID] -> IO BallotBox
ballotBoxVote ballotBox uuids = do
    let newVotes = fromList $ map (\c -> (candidateUUID c, 0)) uuids
    return $ ballotBox { ballotBoxVotes = newVotes }

data Candidate = Candidate {
    candidateName :: String,
    candidateUUID :: UUID
} deriving (Show)

newCandidate :: String -> IO Candidate
newCandidate name = Candidate name <$> UUIDv4.nextRandom

main :: IO ()
main = do
    candidates <- mapM newCandidate ["John", "Jane", "Jim"]



    let ballotBox = newBallotBox candidates
    print ballotBox
