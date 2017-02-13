  {- |
   Offensive AI
     In essence, creates and filters successive lists until either only an empty list remains (indicating a pass)
     or generating a list of moves which are available to be played by the player. First it will check if there is an attack move possible.
     If not it will generate a random move and the move is output to RunGame to continue gameplay.

     This ai was adapted from the random ai.
-}

module AI.Offensive where
import System.Random
import Data.Char
import ApocTools
import Lib.Language
import Lib.Functions
import AI.Random


-- Conducts the successive list builds and filters
aiOffensive :: Chooser
-- Normal move
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
-- PawnPlacement move
aiOffensive gameState PawnPlacement player =
    do
        let coordList = concat $ createCoordList coordinateBoard (theBoard gameState)
        let legalMoves = filterEmpty coordList
        let attackLegalMoves = attackPosition (theBoard gameState) legalMoves player
        -- if a possible move is a move that is diagonal to an enemy (so it can attackPosition
        -- on the next move) then randomly chose one of those
        if (length attackLegalMoves > 0) then
                  do
                     randomNum <- generateRandom (length attackLegalMoves -1)
                     let finalMove = pickElem attackLegalMoves randomNum
                     return (Just [(finalMove)])
        -- else randomly chose any empty piece
        else
                  do
                     randomNum <- generateRandom (length legalMoves -1)
                     let finalMove = pickElem legalMoves randomNum
                     return (Just [(fst finalMove)])


-- Filters the list containing the legal moves
attackMoveList :: [[((Int, Int), Cell)]] -> [[((Int, Int), Cell)]]
attackMoveList [] = []
attackMoveList (x:xs) = filterAttack x : attackMoveList xs

filterAttack :: [((Int, Int), Cell)] -> [((Int, Int), Cell)]
filterAttack x = filter ((/=E).snd) x

-- For pawn placement, takes list of potential PawnPlacement moves and filterPossible
-- those moves that are diagonal to enemy pieces, so they can attack in the next move
attackPosition :: [[Cell]] -> [((Int, Int), Cell)] -> Player -> [(Int, Int)]
attackPosition b [] player = []
attackPosition b (z:zs) player
                               | player == White = do
                                                      let x = (fst $ fst z)
                                                      let y = (snd $ fst z)
                                                      if ((getFromBoard b ((x - 1),(y + 1)) == BK) || (getFromBoard b ((x + 1), (y + 1)) == BK) || (getFromBoard b ((x - 1), (y + 1)) == BP) || (getFromBoard b ((x + 1), (y + 1)) == BP))
                                                        then  (fst z): attackPosition b zs player
                                                        else attackPosition b  zs player
                                | player == Black = do
                                                      let x = (fst $ fst z)
                                                      let y = (snd $ fst z)
                                                      if ((getFromBoard b ((x - 1), (y - 1)) == WK) || (getFromBoard b ((x + 1), (y - 1)) == WK) || (getFromBoard b ((x - 1), (y - 1)) == WP) || (getFromBoard b ((x + 1), (y - 1)) == WP))
                                                         then (fst z): attackPosition b zs player
                                                         else attackPosition b zs player
