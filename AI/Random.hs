-- Random AI code
-- TO DO --
-- Handle pawn PawnPlacement
-- Handle if there are no legal moves


module AI.Random where

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
aiRandom gameState Normal player =
    do
         let coordList = concat $ createCoordList coordinateBoard (theBoard gameState)
         let pieceList = generatePieceList coordList player
         let possibleMoves = filterPossible pieceList player
         let possibleMovesChar = createMoveCharList possibleMoves (theBoard gameState)
         let legalMoves = removeBadPawnMoves pieceList (filterLegal (createCoordList possibleMoves possibleMovesChar) player)
         let cleanedPieceList = removeEmptyPieceList pieceList legalMoves
         let cleanedLegalMoves = removeEmptyLegalMoveList legalMoves
         if (checkPass cleanedPieceList ) then return Nothing
           else do
                   let lengthList = (length cleanedLegalMoves - 1)
                   randomNum <- generateRandom lengthList
                   let finalPiece = pickElem cleanedPieceList randomNum
                   let moveElem = pickElem cleanedLegalMoves randomNum
                   let lengthMoveElem = (length moveElem - 1)
                   randomNum2 <- generateRandom lengthMoveElem
                   let finalMove = pickElem moveElem randomNum2
                   return (Just [(fst finalPiece), (fst finalMove)])
aiRandom gameState PawnPlacement player =
    do
        let coordList = concat $ createCoordList coordinateBoard (theBoard gameState)
        let legalMoves = filterEmpty coordList
        randomNum <- generateRandom (length legalMoves -1)
        let finalMove = pickElem legalMoves randomNum
        return (Just [(fst finalMove)])



removeEmptyPieceList :: [((Int, Int), Cell)] -> [[((Int, Int), Cell)]] -> [((Int, Int), Cell)]
removeEmptyPieceList [] [] = []
removeEmptyPieceList (x:xs) (y:ys) = if ( y == [] ) then removeEmptyPieceList xs ys else x: removeEmptyPieceList xs ys

removeEmptyLegalMoveList ::  [[((Int, Int), Cell)]] -> [[((Int, Int), Cell)]]
removeEmptyLegalMoveList  y = filter (/=[]) y


checkPass :: [a] -> Bool
checkPass list
              | (length list == 0 ) = True
              | otherwise = False



-- Creates a list of pieces for that player and their coordinates
generatePieceList :: [((Int, Int), Cell)] -> Player -> [((Int, Int), Cell)]
generatePieceList coordList player =
              do
                   if player == White then do
                           let kList = filter ((==WK).snd) coordList
                           let pList = filter ((==WP).snd) coordList
                           kList ++ pList
                   else do
                           let kList = filter ((==BK).snd) coordList
                           let pList = filter ((==BP).snd) coordList
                           kList ++ pList

removeBadPawnMoves :: [((Int, Int), Cell)] -> [[((Int, Int), Cell)]] -> [[((Int, Int), Cell)]]
removeBadPawnMoves [] [] = []
removeBadPawnMoves (x:xs) (y:ys) =
                                   if (((snd x) == WK) ||  ((snd x) == BK))
                                       then y : removeBadPawnMoves xs ys
                                   else do
                                           if (isDiagonal x y False) then (removeBadDiagonalMoves x y  : removeBadPawnMoves xs ys)

                                           else (filter ((==E).snd) y) : removeBadPawnMoves xs ys

-- Checks where the a list of possible moves for a coordinate has a diagonal move
isDiagonal :: ((Int, Int), Cell) -> [((Int, Int), Cell)] -> Bool -> Bool
isDiagonal a [] True = True
isDiagonal a [] False = False
isDiagonal a (y:ys) bool = if ((fst $ fst a)  /= (fst $ fst y))  then isDiagonal a ys True
                          else if ( bool == True ) then isDiagonal a ys True else isDiagonal a ys False


removeBadDiagonalMoves :: ((Int, Int), Cell) -> [((Int, Int), Cell)] -> [((Int,Int),Cell)]
removeBadDiagonalMoves a [] = []
removeBadDiagonalMoves a (x:xs) = if ((fst $ fst a)  /= (fst $ fst x) && (snd x /= E) )
                                 then x : removeBadDiagonalMoves a xs
                                 else  if (fst $ fst a)  == (fst $ fst x) then x : removeBadDiagonalMoves a xs else removeBadDiagonalMoves a xs


-- Creates a list of coordinate-piece pairs
createCoordList :: [[(Int, Int)]] -> [[Cell]] -> [[((Int, Int), Cell)]]
createCoordList _ [] = []
createCoordList [] _ = []
createCoordList (x:xs) (y:ys) = zip x y : createCoordList xs ys

-- Creates a list of possible moves for each piece in play
filterPossible :: [((Int, Int), Cell)] -> Player -> [[(Int, Int)]]
filterPossible [] player = []
filterPossible (x:xs) player = if ((snd x) == WP) || ((snd x) == BP) then
                                                    (legalPawnMoves (fst x) player True) : filterPossible xs player
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

-- For PawnPlacement, will remove all moves that are not empty
filterEmpty :: [((Int, Int), Cell)] -> [((Int, Int), Cell)]
filterEmpty [] = []
filterEmpty  (x:xs) = if (snd x == E) then x: filterEmpty xs else filterEmpty xs
