-- Random AI code
-- Will randomly choose a piece to move, and then randomly choose a location to move
-- the piece to. Will also require choosing an illegal move or a pass.

import System.Random 
import Data.Char 

-- Randomly chooses one of those pieces to move
-- Randomly chooses from a legal move, pass, or goof

-- Should also handle the pawn placement?
 
-- Generates two integers within the range of the board
main = do  
    gen <- getStdGen  
    putStrLn $ show $ createMove $ take 2 (randomRs ('0','4') gen)
	
-- Creates the final move to be returned
-- So far, only takes the randomly generated move within the board and creates a tuple
-- Needs to incorporate the randomly chosen piece from the GameState
createMove :: String -> [(Int, Int)]
createMove (x:xs) = zip [digitToInt x] [digitToInt $ head xs]

-- Check GameState for pieces for that player
-- Chooses a random piece to move from the GameState
--checkGameState :: GameState -> Player -> (Int, Int)
--checkGameState GameState Player = (1,0)
  


