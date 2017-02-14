{-|
Module      : Random
Description : CPSC449 W2017 Haskell Apocalypse Assignment
Copyright   : Kowther Hassan, Kaylee Stelter, Matthew Mullins, Saurabh Tomar, Tsz Lam
License     : None
Portability : ghc 7.10.2-3
-}

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

{- |
   Creates and filters successive lists until either only an empty list remains (indicating a pass)
   or generating a list of valid moves which are available to be played by the player. This list is randomly
   chosen from and the move is output to RunGame to continue gameplay
-}
aiRandom :: Chooser
aiRandom gameState Normal player =
    do
         -- Creates a list of coordinate cell pairs representing the gameboard
         let coordList = concat $ createCoordList coordinateBoard (theBoard gameState)
         -- Creates a list of pieces in play (and their coordinates) for the current player
         let pieceList = generatePieceList coordList player
         -- Creates a list of possible moves on the board from the pieceList for the current player
         let possibleMoves = filterPossible pieceList player
         -- Creates a list of pieces which are at the location of each of the possible moves in possibleMoves
         let possibleMovesChar = createMoveCharList possibleMoves (theBoard gameState)
         -- Removes all invalid moves from the list of possible moves for the current player
         let legalMoves = removeBadPawnMoves pieceList (filterLegal (createCoordList possibleMoves possibleMovesChar) player)
         -- Removes all pieces which have no valid moves left in the list of legalMoves
         let cleanedPieceList = removeEmptyPieceList pieceList legalMoves
         -- Removes all elements from the move list which are empty (contain no valid moves)
         let cleanedLegalMoves = removeEmptyLegalMoveList legalMoves
         -- If there are no pieces left in the cleanedPieceList (i.e. there are no valid moves) returns Nothing (a passed move)
         if (checkPass cleanedPieceList ) then return Nothing
           -- Otherwise choose a random piece to move, and a random valid move for it to output
           else do
                   let lengthList = (length cleanedLegalMoves - 1)
                   -- Generates random number to choose a piece to move
                   randomNum <- generateRandom lengthList
                   -- Final piece to be moved is chosen with a random index from the final list of legal pieces
                   let finalPiece = pickElem cleanedPieceList randomNum
                   let moveElem = pickElem cleanedLegalMoves randomNum
                   let lengthMoveElem = (length moveElem - 1)
                   -- Generates random number to choose a move for the chosen piece
                   randomNum2 <- generateRandom lengthMoveElem
                   -- Final move to be made is chosen with a random index from the final list of legal moves
                   let finalMove = pickElem moveElem randomNum2
                   return (Just [(fst finalPiece), (fst finalMove)])

-- If PawnPlacement is passed in as a move type, chooses a random legal coordinate to move to and outputs that move
aiRandom gameState PawnPlacement player =
    do
        let coordList = concat $ createCoordList coordinateBoard (theBoard gameState)
        -- Removes all non-empty coordinates from the list of possible moves on the board
        let legalMoves = filterEmpty coordList
        -- Generates a random number to choose a random legal coordinate to move to
        randomNum <- generateRandom (length legalMoves -1)
        let finalMove = pickElem legalMoves randomNum
        return (Just [(fst finalMove)])

{- |
   Removes any empty cells from a list containing the coordinates of all the pieces for a specific player
-}
removeEmptyPieceList :: [((Int, Int), Cell)] -> [[((Int, Int), Cell)]] -> [((Int, Int), Cell)]
removeEmptyPieceList [] [] = []
removeEmptyPieceList (x:xs) (y:ys) = if ( y == [] ) then removeEmptyPieceList xs ys else x: removeEmptyPieceList xs ys

{- |
Removes any empty cells from a list containing the possible legal moves for each piece
-}
removeEmptyLegalMoveList ::  [[((Int, Int), Cell)]] -> [[((Int, Int), Cell)]]
removeEmptyLegalMoveList  y = filter (/=[]) y

{- |
Checks if a list is empty
-}
checkPass :: [a] -> Bool
checkPass list
              | (length list == 0 ) = True
              | otherwise = False

{- |
Creates a list of pieces for that player and their corresponding coordinates
-}
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

{- |
Removes all the bad pawn (illegal) moves from a list containing possible moves (i.e diagonal moves to an empty cell).
These illegal moves are those which are diagonal moves in which the final destination does not have an enemy piece, or
forward moves for a pawn in which the destination is not empty.
-}
removeBadPawnMoves :: [((Int, Int), Cell)] -> [[((Int, Int), Cell)]] -> [[((Int, Int), Cell)]]
removeBadPawnMoves [] [] = []
removeBadPawnMoves (x:xs) (y:ys) =
                                   if (((snd x) == WK) ||  ((snd x) == BK))
                                       then y : removeBadPawnMoves xs ys
                                   else do
                                           if (isDiagonal x y False) then (removeBadDiagonalMoves x y  : removeBadPawnMoves xs ys)

                                           else (filter ((==E).snd) y) : removeBadPawnMoves xs ys

{- |
Checks a list of possible pawn moves for diagonal moves by checking the differences in x coordinate values.
If a diagonal move exists, True is returned, otherwise False is returned.
-}
isDiagonal :: ((Int, Int), Cell) -> [((Int, Int), Cell)] -> Bool -> Bool
isDiagonal a [] True = True
isDiagonal a [] False = False
isDiagonal a (y:ys) bool = if ((fst $ fst a)  /= (fst $ fst y))  then isDiagonal a ys True
                          else if ( bool == True ) then isDiagonal a ys True else isDiagonal a ys False


{- |
Removes bad diagonal pawn moves (illegal moves) from a list containing possible pawn moves.
A bad diagonal move is one in which no enemy piece is on the destination coordinate.
-}
removeBadDiagonalMoves :: ((Int, Int), Cell) -> [((Int, Int), Cell)] -> [((Int,Int),Cell)]
removeBadDiagonalMoves a [] = []
removeBadDiagonalMoves a (x:xs) = if ((fst $ fst a)  /= (fst $ fst x) && (snd x /= E) )
                                 then x : removeBadDiagonalMoves a xs
                                 else  if (((fst $ fst a)  == (fst $ fst x)) && ((snd x) == E)) then x : removeBadDiagonalMoves a xs else removeBadDiagonalMoves a xs

{- |
Creates a list of coordinate-piece pairs in the form (coordinate, Cell) (eg, ((0,0), BK))
-}
createCoordList :: [[(Int, Int)]] -> [[Cell]] -> [[((Int, Int), Cell)]]
createCoordList _ [] = []
createCoordList [] _ = []
createCoordList (x:xs) (y:ys) = zip x y : createCoordList xs ys

{- |
Creates a list of possible moves for each piece in play for the current player
-}
filterPossible :: [((Int, Int), Cell)] -> Player -> [[(Int, Int)]]
filterPossible [] player = []
filterPossible (x:xs) player = if ((snd x) == WP) || ((snd x) == BP) then
                                                    (legalPawnMoves2 (fst x) player) : filterPossible xs player
                                            else
                                                    (legalKnightMoves (fst x)) : filterPossible xs player

{- |
Creates a list of board pieces present at the destination of each possible move
-}
createMoveCharList :: [[(Int, Int)]] -> [[Cell]] -> [[Cell]]
createMoveCharList [] b = []
createMoveCharList (x:xs) b = innerMoveCharList x b : createMoveCharList xs b

{- |
Aids in the creation of a list of board pieces present at the destination of each possible move
-}
innerMoveCharList :: [(Int, Int)] -> [[Cell]] -> [Cell]
innerMoveCharList [] b = []
innerMoveCharList (x:xs) b = getFromBoard b x : innerMoveCharList xs b

{- |
Filters out moves in which the destination coordinate is one where a friendly piece resides.
-}
filterLegal :: [[((Int, Int), Cell)]] -> Player -> [[((Int, Int), Cell)]]
filterLegal [] player     = []
filterLegal (x:xs) White = filter ((/=WK).snd) (filter ((/=WP).snd) x) : filterLegal xs White
filterLegal (x:xs) Black = filter ((/=BK).snd) (filter ((/=BP).snd) x) : filterLegal xs Black

{- |
Given the length of a list (minus 1), returns a random index within the range of the list.
Used for random index choices to choose pieces and moves.
-}
generateRandom :: Int -> IO Int
generateRandom lengthList = randomRIO (0, lengthList)


{- |
Given a randomly generated index, returns the element at that index.
Used for random index choices to choose pieces and moves.
-}
pickElem :: [a] -> Int -> a
pickElem list index = list !! index

{- |
Used in PawnPlacement moves, removes all non-empty moves from the list of all possible moves.
-}
filterEmpty :: [((Int, Int), Cell)] -> [((Int, Int), Cell)]
filterEmpty [] = []
filterEmpty  (x:xs) = if (snd x == E) then x: filterEmpty xs else filterEmpty xs

{- |
Generates all the possible moves of a pawn on the board given its current position.
For Normal play-type only.
-}
legalPawnMoves2 :: (Int,Int) -> Player -> [(Int,Int)]
-- sX,sY: starting x and y
-- currPlayer: White or Black pawn type
-- knockout: allowed to move diagonally to attack
legalPawnMoves2 (sX,sY) currPlayer
            | (currPlayer == White) = filter possibleMoves [(sX,sY+1),(sX-1,sY+1),(sX+1,sY+1)]
            | (currPlayer == Black) = filter possibleMoves [(sX,sY-1),(sX-1,sY-1),(sX+1,sY-1)]
            where
                possibleMoves (sX,sY) = sX `elem` [0..4] && sY `elem` [0..4]
