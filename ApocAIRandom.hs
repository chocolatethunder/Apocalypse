-- Random AI code
-- Will randomly choose a piece to move, and then randomly choose a location to move
-- the piece to. Will also require choosing an illegal move or a pass.



module ApocAIRandom
       (main,
       createMove) where

import System.Random
import Data.Char
import ApocTools

-- List of coordinates representing the game board
coordinateBoard =  [ [(0,0), (0,1), (0,2), (0,3), (0,4)],
                    [(1,0), (1,1) , (1,2) , (1,3) , (1,4)],
                    [(2,0), (2,1) , (2,2) , (2,3) , (2,4) ],
                    [(3,0), (3,1) , (3,2) , (3,3) , (3,4)],
                    [(4,0), (4,1), (4,2), (4,3), (4,4)] ]


-- Randomly chooses from a legal move, pass, or goof

-- Should also handle the pawn placement

-- Generates two integers within the range of the board
main = do

          gen1 <- getStdGen
          putStrLn $ show $ createMove $ take 2 $ (randomRs ('0','4') gen1)

createCoordBoard :: Board -> [[(Int, Int)]] -> [[((Int, Int), Cell)]]
createCoordBoard [] [] = [[(())]]
createCoordBoard (x:xs) (y:ys) = zip x : createCoordBoard xs ys


pickPiece :: Board -> Player -> (Int, Int)
pickPiece (x:xs) Black =

-- Creates the final move to be returned
-- So far, only takes the randomly generated move within the board and creates a tuple
-- Needs to incorporate the randomly chosen piece from the GameState
createMove :: String -> [(Int, Int)]
createMove (x:xs) = zip [digitToInt x] [digitToInt $ head xs]


-- Checks the GameState and chooses a random piece in play to move
randomAI :: Chooser
randomAI b Normal        c = return (Just [(0,0),(1,2)])
randomAI b PawnPlacement c = return (Just [(2,2)])


-- Check GameState for pieces for that player
-- Chooses a random piece to move from the GameState
--checkGameState :: GameState -> Player -> (Int, Int)
--checkGameState GameState Player = (1,0)
