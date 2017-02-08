module AI.Random where 

import System.Random
import Data.Char

import ApocTools
import Lib.Language
import Lib.Functions

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

aiRandom :: Chooser
-- currBoard is the current board similar to gameBoard above
-- playType is Normal or PawnPlacement
-- playerType is Black or White
aiRandom currBoard playType playerType = do
         let player = White -- to be removed for final version
         let coordList = concat $ createCoordList coordinateBoard gameBoard

         if player == White then do
                                    let wkList = filter ((==WK).snd) coordList
                                    let wpList = filter ((==WP).snd) coordList
                                    let pieceList = wkList ++ wpList
                                    putStrLn $ show $ pieceList
                                    -- change here needs to return in (Just [(Int,Int),(Int,Int)])
                                    return (Just [(2,0),(2,1)])

         else do
                                    let bkList = filter ((==BK).snd) coordList
                                    let bpList = filter ((==BP).snd) coordList
                                    let pieceList = bkList ++ bpList
                                    putStrLn $ show $ pieceList
                                    -- change here needs to return in (Just [(Int,Int),(Int,Int)])
                                    return (Just [(2,0),(2,1)])

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
                                        