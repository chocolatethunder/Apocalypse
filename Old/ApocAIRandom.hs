-- Random AI code
-- Will randomly choose a piece to move, and then randomly choose a location to move
-- the piece to. Will also require choosing an illegal move or a pass.



--module ApocAIRandom
--       (main) where

import System.Random
import Data.Char
import ApocTools


-- List of coordinates representing the game board
coordinateBoard =  [ [(0,0), (0,1), (0,2), (0,3), (0,4)],
                    [(1,0), (1,1) , (1,2) , (1,3) , (1,4)],
                    [(2,0), (2,1) , (2,2) , (2,3) , (2,4) ],
                    [(3,0), (3,1) , (3,2) , (3,3) , (3,4)],
                    [(4,0), (4,1), (4,2), (4,3), (4,4)] ]

-- Sample game board for testing -- to be removed for final version
gameBoard =           [ [WK, WP, WP, WP, WK],
                      [WP, E , E , E , WP],
                      [E , E , E , E , E ],
                      [BP, E , E , E , BP],
                      [BK, BP, BP, BP, BK] ]

-- Creates lists based on the current player and chooses a piece to move
main = do
         let player = White -- to be removed for final version
         let coordList = concat $ createCoordList coordinateBoard gameBoard

         if player == White then do
                                    let wkList = filter ((==WK).snd) coordList
                                    let wpList = filter ((==WP).snd) coordList
                                    let pieceList = wkList ++ wpList
                                    putStrLn $ show $ pieceList
                                    return ()

         else do
                                    let bkList = filter ((==BK).snd) coordList
                                    let bpList = filter ((==BP).snd) coordList
                                    let pieceList = bkList ++ bpList
                                    putStrLn $ show $ pieceList
                                    return ()

-- Creates a list of coordinate-piece pairs
createCoordList :: [[(Int, Int)]] -> [[Cell]] -> [[((Int, Int), Cell)]]
createCoordList _ [] = []
createCoordList [] _ = []
createCoordList (x:xs) (y:ys) = zip x y : createCoordList xs ys

-- Implement function which takes in the generated piece list and returns a randomly chosen piece to move

-- Implement function which generates a random move for the selected type of piece

-- Implement function which handles pawn PawnPlacement

-- Implement a function which randomly chooses between a random move or pass




--- Leftover Functions

          -- Checks the GameState and chooses a random piece in play to move
          --randomAI :: Chooser
          --randomAI b Normal        c = return (Just [(0,0),(1,2)])
          --randomAI b PawnPlacement c = return (Just [(2,2)])

          --gen1 <- getStdGen
          --print gameBoard

          --putStrLn $ show $ createMove $ take 2 $ (randomRs ('0','4') gen1)
