{-|
Module      : Offensive
Description : CPSC449 W2017 Haskell Apocalypse Assignment
Copyright   : Kowther Hassan, Kaylee Stelter, Matthew Mullins, Saurabh Tomar, Tsz Lam
License     : None
Portability : ghc 7.10.2-3
-}

module AI.Offensive where

import System.Random
import Data.Char
import ApocTools
import Lib.Language
import Lib.Functions
import AI.Random

{- |
   Creates and filters successive lists until either only an empty list remains (indicating a pass)
   or generating a list of moves which are available to be played by the player. First it will check if there an attack move is possible.
   Either a normal move in the format Just[(xFrom, yFrom), (xTo, yTo)] or a PawnPlacement move in the format Just[(xTo, yTo)] will be generated.
   If not it will generate a random move. The final move is outputed to RunGame to continue gameplay. This ai was adapted from the Random ai.
-}
aiOffensive :: Chooser
aiOffensive gameState Normal player =
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

       --- attack move-------------------------
       -- Generates a sub list of attack moves from the legalMoves list
       let attackableMovesList = attackMoveList legalMoves
       -- cleans up attackMoveList
       let attackableCleanedMovesList = removeEmptyLegalMoveList attackableMovesList
       -- cleans up pieceList
       let attackableCleanedPieceList = removeEmptyPieceList pieceList attackableMovesList
       -- checks if moves list is not empty and piece list is not empty
       attackOrRandom <- generateRandom 9
       if ( (checkPass attackableCleanedPieceList == False) && (attackOrRandom /= 1) && (checkPass attackableCleanedMovesList == False)) then
                             do
                               -- generates random move from attackable moves list and outputs it in the correct format
                                let lengthList = (length attackableCleanedMovesList - 1)
                                -- Generates random number to choose a piece to move
                                randomNum <- generateRandom lengthList
                                -- Final piece to be moved is chosen with a random index from the final list of legal pieces
                                let finalPiece = pickElem attackableCleanedPieceList randomNum
                                let moveElem = pickElem attackableCleanedMovesList randomNum
                                let lengthMoveElem = (length moveElem - 1)
                                -- Generates random number to choose a move for the chosen piece
                                randomNum2 <- generateRandom lengthMoveElem
                                -- Final move to be made is chosen with a random index from the final list of legal moves
                                let finalMove = pickElem moveElem randomNum2
                                return (Just [(fst finalPiece), (fst finalMove)])

       -- If there are no pieces left in the cleanedPieceList (i.e. there are no valid moves) returns Nothing (a passed move)
       else
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
aiOffensive gameState PawnPlacement player =
  do
      let coordList = concat $ createCoordList coordinateBoard (theBoard gameState)
      -- Removes all non-empty coordinates from the list of possible moves on the board
      let legalMoves = filterEmpty coordList
      -- Generates a random number to choose a random legal coordinate to move to
      randomNum <- generateRandom (length legalMoves -1)
      let finalMove = pickElem legalMoves randomNum
      return (Just [(fst finalMove)])




{- |
Removes all non-empty moves from the move list
-}
attackMoveList :: [[((Int, Int), Cell)]] -> [[((Int, Int), Cell)]]
attackMoveList [] = []
attackMoveList (x:xs) = filterAttack x : attackMoveList xs

{-|
Aids in removing all non-empty moves from the move list
-}
filterAttack :: [((Int, Int), Cell)] -> [((Int, Int), Cell)]
filterAttack x = filter ((/=E).snd) x
