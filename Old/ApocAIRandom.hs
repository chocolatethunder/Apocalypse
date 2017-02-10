-- Random AI code
-- OLD CODE -- DO NOT MODIFY
--module AI.Random where

import System.Random
import Data.Char
import ApocTools
import Lib.Language
import Lib.Functions

-- List of coordinates representing the game board
coordinateBoard =  [ [(0,0), (1,0), (2,0), (3,0), (4,0)],
                    [(0,1), (1,1) , (2,1) , (3,1) , (4,1)],
                    [(0,2), (1,2) , (2,2) , (3,2) , (4,2) ],
                    [(0,3), (1,3) , (2,3) , (3,3) , (4,3)],
                    [(0,4), (1,4), (2,4), (3,4), (4,4)] ]


-- Creates lists based on the current player and chooses a piece to move
aiRandom :: Chooser
aiRandom gameState Normal player do =
         --let player = White -- to be removed for final version
         let coordList = concat $ createCoordList coordinateBoard (theBoard gameBoard)
         if player == White then do
                                    let kList = filter ((==WK).snd) coordList
                                    let pList = filter ((==WP).snd) coordList
                            else do
                                    let kList = filter ((==BK).snd) coordList
                                    let pList = filter ((==BP).snd) coordList

         let pieceList = kList ++ pList
         let possibleMoves = filterPossible pieceList player
         let possibleMovesChar = createMoveCharList possibleMoves (theBoard gameBoard)
         let legalMoves = filterLegal (createCoordList possibleMoves possibleMovesChar) player
         let lengthList = (length legalMoves - 1)
         randomNum <- generateRandom lengthList
         let finalPiece = pickElem pieceList randomNum
         let moveElem = pickElem legalMoves randomNum
         let lengthMoveElem = (length moveElem - 1)
         randomNum2 <- generateRandom lengthMoveElem
         let finalMove = pickElem moveElem randomNum2
         return (Just [(fst finalPiece), (fst finalMove)])


-- Creates a list of coordinate-piece pairs
createCoordList :: [[(Int, Int)]] -> [[Cell]] -> [[((Int, Int), Cell)]]
createCoordList _ [] = []
createCoordList [] _ = []
createCoordList (x:xs) (y:ys) = zip x y : createCoordList xs ys

-- Creates a list of possible moves for each piece in play
filterPossible :: [((Int, Int), Cell)] -> Player -> [[(Int, Int)]]
filterPossible [] player = []
filterPossible (x:xs) player = if ((snd x) == WP) || ((snd x) == BP) then
                                                    (legalPawnMoves (fst x) White True) : filterPossible xs player
                                            else
                                                    (legalKnightMoves (fst x)) : filterPossible xs player

-- Creates a list of board pieces present at the destination of each possible move
createMoveCharList :: [[(Int, Int)]] -> [[Cell]] -> [[Cell]]
createMoveCharList [] b = []
createMoveCharList (x:xs) b = innerMoveCharList x b : createMoveCharList xs b

-- Aids in the creation of a list of board pieces present at the destination of each possible move
innerMoveCharList :: [(Int, Int)] -> [[Cell]] -> [Cell]
innerMoveCharList [] b = []
innerMoveCharList (x:xs) b = getFromBoard b x : innerMoveCharList xs b

-- Filters out non-legal moves from the list of possible moves
filterLegal :: [[((Int, Int), Cell)]] -> Player -> [[((Int, Int), Cell)]]
filterLegal [] player     = []
filterLegal (x:xs) White = filter ((/=WK).snd) (filter ((/=WP).snd) x) : filterLegal xs White
filterLegal (x:xs) Black = filter ((/=BK).snd) (filter ((/=BP).snd) x) : filterLegal xs Black


-- Given the length of a list (minus 1), returns a random index within the range of the list
generateRandom :: Int -> IO Int
generateRandom lengthList = randomRIO (0, lengthList)

-- Given a randomly generated index, returns the element at that index
pickElem :: [a] -> Int -> a
pickElem list index = list !! index


-- Implement function which handles pawn PawnPlacement
