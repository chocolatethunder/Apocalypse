module AI.Offensive where

--import ApocTools
--import Lib.Language
--import Lib.Functions

--aiOffensive :: Chooser
--aiOffensive currBoard Normal playerType = return (Just [(2,0),(2,1)])

--Check playerType
  --if BLACK
    --if (BK == 0)
      --if possiblePawnCapture
        --select random possiblePawnCapture
        --RETURN
      --else no possiblePawnCapture
        --select random WP movement
        --RETURN
    --else (BK != 0)
      --if possibleKnightCapture
        --select random possibleKnightCapture
        --RETURN
      --else no possibleKnightCapture
        --if possiblePawnCapture
          --select random possiblePawnCapture
          --RETURN
        --else no possiblePawnCapture
          --select random WP movement
          --RETURN

  --else WHITE

  -- Offensive AI
  -- In essence, creates and filters successive lists until either only an empty list remains (indicating a pass)
  -- or generating a list of moves which are available to be played by the player.
  -- first it will check if there is an attack move possible. If not it will generate a random move.
  -- This list is randomly chosen from and the move is output to RunGame to continue gameplay

--  module AI.ApocAI2 where

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

-- Conducts the successive list builds and filters
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
         let attackableMovesList = attackMoveList legalMoves
         let attackableCleanedMovesList = removeEmptyLegalMoveList attackableMovesList
         let attackableCleanedPieceList = removeEmptyPieceList pieceList attackableMovesList
         if ( (checkPass attackableCleanedPieceList == False) && (checkPass attackableCleanedMovesList == False)) then
                               do
                                  let lengthList = (length attackableCleanedMovesList - 1)
                                  randomNum <- generateRandom lengthList
                                  let finalPiece = pickElem attackableCleanedPieceList randomNum
                                  let moveElem = pickElem attackableCleanedMovesList randomNum
                                  let lengthMoveElem = (length moveElem - 1)
                                  randomNum2 <- generateRandom lengthMoveElem
                                  let finalMove = pickElem moveElem randomNum2
                                  return (Just [(fst finalPiece), (fst finalMove)])
     -- end attackmove---------------------
     -- pass move ----------------------------
          -- If there are no pieces left in the cleanedPieceList (i.e. there are no valid moves) returns Nothing (a passed move)
         else
             if (checkPass cleanedPieceList ) then return Nothing
     -- end pass move ---------------------------------
     -- random legal move ------------------------------------
           -- Otherwise choose a random piece to move, and a random valid move for it to output
             else do
                   let lengthList = (length cleanedLegalMoves - 1)
                   randomNum <- generateRandom lengthList
                   let finalPiece = pickElem cleanedPieceList randomNum
                   let moveElem = pickElem cleanedLegalMoves randomNum
                   let lengthMoveElem = (length moveElem - 1)
                   randomNum2 <- generateRandom lengthMoveElem
                   let finalMove = pickElem moveElem randomNum2
                   return (Just [(fst finalPiece), (fst finalMove)])
aiOffensive gameState PawnPlacement player =
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



attackMoveList :: [[((Int, Int), Cell)]] -> [[((Int, Int), Cell)]]
attackMoveList [] = []
attackMoveList (x:xs) = filterAttack x : attackMoveList xs


filterAttack :: [((Int, Int), Cell)] -> [((Int, Int), Cell)]
filterAttack x = filter ((/=E).snd) x
