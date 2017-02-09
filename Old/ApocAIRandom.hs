-- Random AI code
-- Will randomly choose a piece to move, and then randomly choose a location to move
-- the piece to. Will also require choosing an illegal move or a pass.



--module ApocAIRandom
--       (main) where

import System.Random
import Data.Char
import ApocTools



-- List of coordinates representing the game board
coordinateBoard =  [ [(0,0), (1,0), (2,0), (3,0), (4,0)],
                    [(0,1), (1,1) , (2,1) , (3,1) , (4,1)],
                    [(0,2), (1,2) , (2,2) , (3,2) , (4,2) ],
                    [(0,3), (1,3) , (2,3) , (3,3) , (4,3)],
                    [(0,4), (1,4), (2,4), (3,4), (4,4)] ]

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
                                    let possibleMoves = filterPossible pieceList "White"
                                    let possibleMovesChar = createMoveCharList possibleMoves gameBoard
                                    let legalMoves = filterLegal (createCoordList possibleMoves possibleMovesChar) "White"
                                    putStrLn $ show $ pieceList
                                    putStrLn $ show $ legalMoves
                                    return ()

         else do
                                    let bkList = filter ((==BK).snd) coordList
                                    let bpList = filter ((==BP).snd) coordList
                                    let pieceList = bkList ++ bpList
                                    let possibleMoves = filterPossible pieceList "Black"
                                    let possibleMovesChar = createMoveCharList possibleMoves gameBoard
                                    let legalMoves = filterLegal (createCoordList possibleMoves possibleMovesChar) "Black"
                                    putStrLn $ show $ (createCoordList possibleMoves possibleMovesChar)
                                    putStrLn $ show $ legalMoves
                                    return ()

-- Creates a list of coordinate-piece pairs
createCoordList :: [[(Int, Int)]] -> [[Cell]] -> [[((Int, Int), Cell)]]
createCoordList _ [] = []
createCoordList [] _ = []
createCoordList (x:xs) (y:ys) = zip x y : createCoordList xs ys

-- Creates a list of possible moves for each piece in play
filterPossible :: [((Int, Int), Cell)] -> String -> [[(Int, Int)]]
filterPossible [] "White" = []
filterPossible (x:xs) "White" = if ((snd x) == WP) || ((snd x) == BP) then
                                                    (legalPawnMoves (fst x) White True) : filterPossible xs "White"
                                            else
                                                    (legalKnightMoves (fst x)) : filterPossible xs "White"

-- Creates a list of board pieces present at the destination of each possible move
createMoveCharList :: [[(Int, Int)]] -> [[Cell]] -> [[Cell]]
createMoveCharList [] b = []
createMoveCharList (x:xs) b = innerMoveCharList x b : createMoveCharList xs b

-- Aids in the creation of a list of board pieces present at the destination of each possible move
innerMoveCharList :: [(Int, Int)] -> [[Cell]] -> [Cell]
innerMoveCharList [] b = []
innerMoveCharList (x:xs) b = getFromBoard b x : innerMoveCharList xs b

-- Filters out non-legal moves from the list of possible moves
filterLegal :: [[((Int, Int), Cell)]] -> String -> [[((Int, Int), Cell)]]
filterLegal [] "White"     = []
filterLegal (x:xs) "White" = filter ((/=WK).snd) (filter ((/=WP).snd) x) : filterLegal xs "White"










-- Generates all the possible moves of a knight on the board given it's current position. For Normal playtype only.
legalKnightMoves :: (Int,Int) -> [(Int,Int)]
legalKnightMoves (sX,sY) = filter possibleMoves [(sX+2,sY+1),(sX+2,sY-1),(sX-2,sY+1),(sX-2,sY-1),(sX+1,sY+2),(sX+1,sY-2),(sX-1,sY+2),(sX-1,sY-2)] where possibleMoves (sX,sY) = sX `elem` [0..4] && sY `elem` [0..4]



-- Generates all the possible moves of a pawn on the board given it's current position. For Normal playtype only.
legalPawnMoves :: (Int,Int) -> Player -> Bool -> [(Int,Int)]
-- sX,sY: starting x and y
-- currPlayer: White or Black pawn type
-- knockout: allowed to move diagonally to attack
legalPawnMoves (sX,sY) currPlayer knockout
            | (knockout == True && currPlayer == White) = filter possibleMoves [(sX,sY+1),(sX-1,sY+1),(sX+1,sY+1)]
            | (knockout == False && currPlayer == White) = filter possibleMoves [(sX,sY+1)]
            | (knockout == True && currPlayer == Black) = filter possibleMoves [(sX,sY-1),(sX-1,sY-1),(sX+1,sY-1)]
            | (knockout == False && currPlayer == Black) = filter possibleMoves [(sX,sY-1)]
            where
                possibleMoves (sX,sY) = sX `elem` [0..4] && sY `elem` [0..4]






-- Implement function which chooses one of those pieces with a valid move (or passes if there are none)

-- Implement function which generates a move for the chosen piece

-- Implement function which handles pawn PawnPlacement






--- Leftover Functions

          -- Checks the GameState and chooses a random piece in play to move
          --randomAI :: Chooser
          --randomAI b Normal        c = return (Just [(0,0),(1,2)])
          --randomAI b PawnPlacement c = return (Just [(2,2)])

          --gen1 <- getStdGen
          --print gameBoard

          --putStrLn $ show $ createMove $ take 2 $ (randomRs ('0','4') gen1)
