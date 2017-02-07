-- Random AI code
-- Will randomly choose a piece to move, and then randomly choose a location to move
-- the piece to. Will also require choosing an illegal move or a pass.



module ApocAIRandom 
       (main, 
       createMove) where 

import System.Random 
import Data.Char
import ApocTools

-- Randomly chooses one of those pieces to move
-- Randomly chooses from a legal move, pass, or goof

-- Should also handle the pawn placement?
 
-- Generates two integers within the range of the board
main = do  

          gen1 <- getStdGen
          putStrLn $ show $ createMove $ take 2 $ (randomRs ('0','4') gen1)



-- Creates the final move to be returned
-- So far, only takes the randomly generated move within the board and creates a tuple
-- Needs to incorporate the randomly chosen piece from the GameState
createMove :: String -> [(Int, Int)]
createMove (x:xs) = zip [digitToInt x] [digitToInt $ head xs]


-- Checks the GameState and chooses a random piece in play to move
pickPiece :: [(Integer, Integer)]
pickPiece = [(0,0)]


-- Check GameState for pieces for that player
-- Chooses a random piece to move from the GameState
--checkGameState :: GameState -> Player -> (Int, Int)
--checkGameState GameState Player = (1,0)
  


