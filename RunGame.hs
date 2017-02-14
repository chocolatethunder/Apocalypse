{-|
Module      : RunGame
Description : CPSC449 W2017 Haskell Apocalypse Assignment
Copyright   : Kowther Hassan, Kaylee Stelter, Matthew Mullins, Saurabh Tomar, Tsz Lam
License     : None
Portability : ghc 7.10.2-3
-}

{- |
   Runs the game until the endgame scenario is reached.
   It checks and categorizes each move made by the user/computer and then updates the game state.
-}
module RunGame (gameLoop) where

import Data.Maybe
import Control.Monad
import ApocTools
import Language
import Functions
import ApocStrategyHuman
import Offensive
import Random

{- |
Main gameplay loop which continues to run as long as the endgame conditions have not been met
-}
gameLoop :: GameState -> [Char] -> [Char] -> PlayType -> Bool -> IO()
gameLoop currBoard bl wt playType endGame = do
                                if (endGame == False) then
                                    do
                                        -- check if a player has run out of pawns
                                        let blackPawnsLeft = arePawnsLeft (theBoard currBoard) Black
                                        let whitePawnsLeft = arePawnsLeft (theBoard currBoard) White

                                        -- store the current penalty points
                                        let blackPenalty = (blackPen currBoard)
                                        let whitePenalty = (whitePen currBoard)

                                        -- END GAME CHECK
                                            -- End game conditions:
                                            --      One of the players looses all his/her pawns.  The other player is the winner.
                                            --      One of the players accumulates two penalty points.  The other player is the winner.
                                        if (blackPawnsLeft == False || whitePawnsLeft == False || blackPenalty >= 2 || whitePenalty >= 2) then
                                            gameLoop currBoard bl wt Normal True
                                        else
                                            do
                                                -- get the data from the command line
                                                -- incoming data is a Maybe [(Int,Int)] or Nothing for a pass
                                                blackMove <- (getPlayerMove currBoard bl playType Black) -- Raw incoming data
                                                whiteMove <- (getPlayerMove currBoard wt playType White) -- Raw incoming data

                                                --  Both players pass on the same round. The one with the most pawns wins.
                                                if ((blackMove == Nothing && whiteMove == Nothing)) then
                                                    do
                                                        let newBoard = GameState (Passed)
                                                                                  (blackPenalty)
                                                                                  (Passed)
                                                                                  (whitePenalty)
                                                                                  (theBoard currBoard)
                                                        gameLoop newBoard bl wt Normal True
                                                else
                                                    -- BEGIN OTHER CHECKS
                                                    do
                                                        -- check if the move is valid by black. Note: A pass will return a False
                                                        isBlackMoveValid <- validateMove currBoard blackMove Black

                                                        -- set the played type for black
                                                        let blackPlayed = case blackMove of
                                                                                        Nothing -> Passed
                                                                                        maybe | (isBlackMoveValid == False) -> Goofed (getTwoCoords blackMove)
                                                                                              | (isBlackMoveValid == True) -> Played (getTwoCoords blackMove)
                                                        -- check and set penalties
                                                        let newBlackPenalty = case blackMove of
                                                                                        Nothing -> blackPenalty
                                                                                        maybe | (isBlackMoveValid == False) -> succ blackPenalty
                                                                                              | (isBlackMoveValid == True) -> blackPenalty

                                                        -- check if the move is valid by white. Note: A pass will return a False
                                                        isWhiteMoveValid <- validateMove currBoard whiteMove White

                                                        -- set the played type for white
                                                        let whitePlayed = case whiteMove of
                                                                                        Nothing -> Passed
                                                                                        maybe | (isWhiteMoveValid == False) -> Goofed (getTwoCoords whiteMove)
                                                                                              | (isWhiteMoveValid == True) -> Played (getTwoCoords whiteMove)
                                                        -- check and set penalties
                                                        let newWhitePenalty = case whiteMove of
                                                                                        Nothing -> whitePenalty
                                                                                        maybe | (isWhiteMoveValid == False) -> succ whitePenalty
                                                                                              | (isWhiteMoveValid == True) -> whitePenalty

                                                        -- Collision detection and board update here
                                                        updatedBoard <- updateGameBoard (theBoard currBoard) isBlackMoveValid isWhiteMoveValid  blackMove whiteMove

                                                        -- Save game state here
                                                        let newBoard = GameState (blackPlayed)
                                                                                  (newBlackPenalty)
                                                                                  (whitePlayed)
                                                                                  (newWhitePenalty)
                                                                                  (updatedBoard)
                                                                                  
                                                        if (blackPawnsLeft == False || whitePawnsLeft == False || newBlackPenalty >= 2 || newWhitePenalty >= 2) then 
                                                            
                                                            do 
                                                                -- Loop back
                                                                putStrLn (show(newBoard))
                                                                gameLoop newBoard bl wt Normal True
                                                            
                                                        else
                                                        
                                                            do 
                                                                -- Check if next round is a "Normal" round or a "PawnPlacement" round
                                                                let isBlackPawnAtEnd = isPawnAtEnd updatedBoard blackMove
                                                                let isWhitePawnAtEnd = isPawnAtEnd updatedBoard whiteMove
                                                                updatedBoard' <- checkPawnUpgrade newBoard isBlackPawnAtEnd isWhitePawnAtEnd bl wt blackMove whiteMove
                                                                -- Loop back
                                                                putStrLn (show(updatedBoard'))
                                                                gameLoop updatedBoard' bl wt Normal False
                                                            
                                else
                                    do
                                        let blackpenalty = (blackPen currBoard)
                                        let whitepenalty = (whitePen currBoard)
                                        let blackPawnsLeft = (getPawnsLeft (theBoard currBoard) Black)
                                        let whitePawnsLeft = (getPawnsLeft (theBoard currBoard) White)
                                        endTheGame blackpenalty whitepenalty blackPawnsLeft whitePawnsLeft
--PAWNPLACEMENT MODE

{- |
Checks the GameState to see if a pawn upgrade is required.
-}
checkPawnUpgrade :: GameState -> Bool -> Bool -> [Char] -> [Char] -> Maybe [(Int,Int)] -> Maybe [(Int,Int)] -> IO GameState
checkPawnUpgrade currBoard isBlackPawnAtEnd isWhitePawnAtEnd bl wt blackMove whiteMove
    | (isBlackPawnAtEnd == True || isWhitePawnAtEnd == True) = do
                                                                newBoard <- upgradePawn currBoard bl wt blackMove whiteMove
                                                                return newBoard
    | otherwise = return(currBoard)

{- |
Checks to see if a pawn is on the end of the opposing side of the board.
Used in determining if a pawn upgrade/placement is required.
-}
isPawnAtEnd :: Board -> Maybe [(Int,Int)] -> Bool
isPawnAtEnd gBoard move
    | (finalY == 4 && movingUnit == WP) = True
    | (finalY == 0 && movingUnit == BP) = True
    | otherwise = False
    where
        finalY = snd(snd(getTwoCoords(move)))
        movingUnit = getFromBoard gBoard (snd(getTwoCoords(move)))

{- |
Upgrades a pawn to a knight during gameplay.
-}
upgradePawn :: GameState -> [Char] -> [Char] -> Maybe [(Int,Int)] -> Maybe [(Int,Int)] -> IO GameState
upgradePawn currBoard bl wt blackMove whiteMove = do

    let finalBY = snd(snd(getTwoCoords(blackMove)))
    let finalWY = snd(snd(getTwoCoords(whiteMove)))
    let movingBUnit = getFromBoard (theBoard currBoard) (snd(getTwoCoords(blackMove)))
    let movingWUnit = getFromBoard (theBoard currBoard) (snd(getTwoCoords(whiteMove)))
    let blackKnightsLeft = getKnightsLeft (theBoard currBoard) Black
    let whiteKnightsLeft = getKnightsLeft (theBoard currBoard) White
    -- auto upgrade conditions
    let canUpgradeBlack = (finalBY == 0 && movingBUnit == BP && blackKnightsLeft > 0 && blackKnightsLeft < 2)
    let canUpgradeWhite = (finalWY == 4 && movingWUnit == WP && whiteKnightsLeft > 0 && whiteKnightsLeft < 2)
    -- check if they can move
    let canBlackMovePawn = (finalBY == 0 && movingBUnit == BP && blackKnightsLeft >= 2)
    let canWhiteMovePawn = (finalWY == 4 && movingWUnit == WP && whiteKnightsLeft >= 2)
    -- only if a black unit has to upgrade
    newBoard <- case canUpgradeBlack of True -> do
                                                    let blackplay = UpgradedPawn2Knight(fst(snd(getTwoCoords(blackMove))),snd(snd(getTwoCoords(blackMove))))
                                                    let whiteplay = (whitePlay currBoard)
                                                    let blackPenalty = (blackPen currBoard)
                                                    let whitePenalty = (whitePen currBoard)
                                                    let uBoard = (replace2 (theBoard currBoard) (snd(getTwoCoords(blackMove))) BK)
                                                    -- update to the new gamestate
                                                    let newGameState = GameState (blackplay)
                                                                                (blackPenalty)
                                                                                (whiteplay)
                                                                                (whitePenalty)
                                                                                (uBoard)
                                                    -- return the default gamestate
                                                    return newGameState
                                        False -> do
                                                    newGameState <- case canBlackMovePawn of -- True -> pawnMoveMode currBoard canBlackMovePawn canWhiteMovePawn bl wt blackMove whiteMove
                                                                                             True -> pawnMoveMode currBoard bl Black blackMove
                                                                                             False -> return(currBoard)
                                                    -- return the default gamestate
                                                    return newGameState
    -- only if a white unit has to upgrade
    newBoard' <- case canUpgradeWhite of True -> do
                                                    let blackplay = (blackPlay currBoard)
                                                    let whiteplay = UpgradedPawn2Knight(fst(snd(getTwoCoords(whiteMove))),snd(snd(getTwoCoords(whiteMove))))
                                                    let blackPenalty = (blackPen currBoard)
                                                    let whitePenalty = (whitePen currBoard)
                                                    let uBoard = (replace2 (theBoard newBoard) (snd(getTwoCoords(whiteMove))) WK)
                                                    -- update to the new gamestate
                                                    let newGameState = GameState (blackplay)
                                                                                (blackPenalty)
                                                                                (whiteplay)
                                                                                (whitePenalty)
                                                                                (uBoard)
                                                    -- return the default gamestate
                                                    return newGameState
                                         False -> do
                                                    newGameState <- case canWhiteMovePawn of -- True -> pawnMoveMode newBoard canBlackMovePawn canWhiteMovePawn bl wt blackMove whiteMove
                                                                                             True -> pawnMoveMode currBoard wt White whiteMove
                                                                                             False -> return(newBoard)
                                                    -- return the default gamestate
                                                    return newGameState
    return newBoard'

{- |
Handles PawnPlacement during gameplay.
-}
pawnMoveMode :: GameState -> [Char] -> Player -> Maybe [(Int,Int)] -> IO GameState
pawnMoveMode currBoard stratType player move = do
                                            upgradeTo <- (getPlayerMove currBoard stratType PawnPlacement player) -- Raw incoming data
                                            let destPiece = getFromBoard (theBoard currBoard) (getACoord(upgradeTo))
                                            let newMove = case (destPiece /= E) of True -> BadPlacedPawn ((snd(getTwoCoords(move))),(getACoord(upgradeTo)))
                                                                                   False -> PlacedPawn ((snd(getTwoCoords(move))),(getACoord(upgradeTo)))
                                            let blackPenalty = (blackPen currBoard)
                                            let whitePenalty = (whitePen currBoard)
                                            -- assign new penalties
                                            let newBlackPenalty = case (destPiece /= E && player == Black) of True -> succ blackPenalty
                                                                                                              False -> blackPenalty
                                            let newWhitePenalty = case (destPiece /= E && player == White) of True -> succ whitePenalty
                                                                                                              False -> whitePenalty
                                            -- update the game board with new placements
                                            uBoard <- case (destPiece == E && player == Black) of True -> movePlayer (theBoard currBoard) BP (snd(getTwoCoords(move))) (getACoord(upgradeTo))
                                                                                                  False -> return (theBoard currBoard)
                                            uBoard' <- case (destPiece == E && player == White) of True -> movePlayer uBoard WP (snd(getTwoCoords(move))) (getACoord(upgradeTo))
                                                                                                   False -> return uBoard
                                            -- update the Played type
                                            let blackplay = case player of Black -> newMove
                                                                           White -> (blackPlay currBoard)

                                            let whiteplay = case player of Black -> (whitePlay currBoard)
                                                                           White -> newMove
                                            -- construct a new game state
                                            let newGameState = GameState (blackplay)
                                                                        (newBlackPenalty)
                                                                        (whiteplay)
                                                                        (newWhitePenalty)
                                                                        (uBoard)
                                            -- return the default gamestate
                                            return newGameState
--STRATEGY CHANNELING SYSTEM

{- |
Determines what playerType is conducting the current move and then chooses a move
based on the playerType.
-}
getPlayerMove :: GameState -> [Char] -> PlayType -> Player -> IO (Maybe [(Int,Int)])
-- currBoard: current Gamestate data
-- stratType: type of player/strat coming in
-- playType: Normal or PawnPlacement
-- playerType: Black or White player making the play
getPlayerMove currBoard stratType playType playerType
    | stratType == "human" = humanPlayer currBoard playType playerType
    | stratType == "computer" = aiMove currBoard playType playerType "offensive"
    | stratType == "random" = aiMove currBoard playType playerType "random"
    | otherwise = return (Just [(0,0)]) -- this needs to error out. To do

{- |
Determines the type of AI which is being called, and calls the appropriate
functions in the AI modules to retrieve a move.
-}
aiMove :: GameState -> PlayType -> Player -> [Char] -> IO (Maybe [(Int,Int)])
aiMove currBoard playType playerType aiType
    | aiType == "offensive" = aiOffensive currBoard playType playerType
    | aiType == "random" = aiRandom currBoard playType playerType
    | otherwise = return(Just [(0,0)]) -- this needs to error out. To do

--COLLISION DETECTION FUNCTIONS
{- |
Updates the game board with the validated moves.
-}
updateGameBoard :: Board -> Bool -> Bool -> Maybe [(Int,Int)] -> Maybe [(Int,Int)] -> IO Board
updateGameBoard gBoard isBlackMoveValid isWhiteMoveValid blackMove whiteMove
    | (isBlackMoveValid == True && isWhiteMoveValid == False) = collision gBoard blackMove Nothing
    | (isBlackMoveValid == False && isWhiteMoveValid == True) = collision gBoard Nothing whiteMove
    | (isBlackMoveValid == True && isWhiteMoveValid == True) = collision gBoard blackMove whiteMove
    | otherwise = collision gBoard Nothing Nothing

{- |
Detects and adjusts for collisions during gameplay.
-}
collision :: Board -> Maybe [(Int,Int)] -> Maybe [(Int,Int)] -> IO Board
collision gBoard bPos wPos
    -- Both players move AND there IS collision between them
    | (bPos /= Nothing && wPos /= Nothing && playerCollision == True && playerSwap == False) = do
                                                                            -- The black piece, the white piece, and the one that is already there compete
                                                                            let winner = playerStack [(getFromBoard gBoard bToPos),(getFromBoard gBoard bFromPos),(getFromBoard gBoard wFromPos)] True
                                                                            -- move the black player. The player to move here is arbitary just as long as
                                                                            -- the opposite player has it;s original loction cleaned up
                                                                            newBoard <- movePlayer gBoard winner bFromPos bToPos
                                                                            -- clear the white player's original position
                                                                            let newBoard' = clearPiece newBoard wFromPos
                                                                            -- return the updated board
                                                                            return newBoard'
    -- Both players move WITHOUT any between them
    | (bPos /= Nothing && wPos /= Nothing && playerCollision == False && playerSwap == False) = do
                                                                            
                                                                            let wPiece = (getFromBoard gBoard wFromPos)
                                                                            let bPiece = (getFromBoard gBoard bFromPos)
                                                                            
                                                                            let a = clearPiece gBoard bFromPos
                                                                            let b = clearPiece a wFromPos
                                                                            
                                                                            let bwinner = playerStack [bPiece,(getFromBoard b bToPos)] False
                                                                            let newBoard = replace2 b bToPos bwinner
                                                                            
                                                                            let wwinner = playerStack [wPiece,(getFromBoard newBoard wToPos)] False
                                                                            let newBoard' = replace2 newBoard wToPos wwinner
                                                                            
                                                                            {-
                                                                            -- move the white piece
                                                                            let wwinner = playerStack [wPiece,(getFromBoard b wToPos)] False                                                                            
                                                                            
                                                                            --putStrLn(show((getFromBoard gBoard wFromPos)))
                                                                            --putStrLn(show((getFromBoard gBoard wToPos)))
                                                                            putStrLn("white move winner" ++ show(wwinner))
                                                                            
                                                                            newBoard <- movePlayer b wwinner wFromPos wToPos
                                                                            
                                                                            -- move the black piece
                                                                            let bwinner = playerStack [bPiece,(getFromBoard newBoard bToPos)] False
                                                                            
                                                                            --putStrLn(show((getFromBoard newBoard bFromPos)))
                                                                            --putStrLn(show((getFromBoard newBoard bToPos)))
                                                                            putStrLn("black move winner" ++ show(bwinner))
                                                                            
                                                                            newBoard' <- movePlayer newBoard bwinner bFromPos bToPos
                                                                            
                                                                            -- return the updated board
                                                                            -}
                                                                            return newBoard'
                                                                            
    -- Both players swap
    | (bPos /= Nothing && wPos /= Nothing && playerCollision == False && playerSwap == True) = do
                                                                            -- swap
                                                                            let wPiece = (getFromBoard gBoard wFromPos)
                                                                            let bPiece = (getFromBoard gBoard bFromPos)
                                                                            let newBoard = replace2 gBoard bToPos bPiece
                                                                            let newBoard' = replace2 newBoard wToPos wPiece
                                                                            -- return the updated board
                                                                            return newBoard'
    -- if both players fail
    | (bPos == Nothing && wPos == Nothing) = return gBoard
    -- only White player moves
    | (bPos == Nothing) = do
                            let winner = playerStack [(getFromBoard gBoard wFromPos),(getFromBoard gBoard wToPos)] False
                            newBoard <- movePlayer gBoard winner wFromPos wToPos
                            return newBoard
    -- only Black player moves
    | (wPos == Nothing) = do
                            let winner = playerStack [(getFromBoard gBoard bFromPos),(getFromBoard gBoard bToPos)] False
                            newBoard <- movePlayer gBoard winner bFromPos bToPos
                            return newBoard
    where
        -- Piece positions
        bFromPos = fst(getTwoCoords bPos)
        wFromPos = fst(getTwoCoords wPos)
        bToPos = snd(getTwoCoords bPos)
        wToPos = snd(getTwoCoords wPos)
        playerCollision = (fst(bToPos) == fst(wToPos) && snd(bToPos) == snd(wToPos))
        playerSwap = (fst(bFromPos) == fst(wToPos) && snd(bFromPos) == snd(wToPos) && fst(wFromPos) == fst(bToPos) && snd(wFromPos) == snd(bToPos))

{- |
Moves a piece on the board to a specified location.
-}
movePlayer :: Board -> Cell -> (Int,Int) -> (Int,Int) -> IO Board
movePlayer gBoard piece from to = do
                                   -- update to position
                                   let updatedBoard = replace2 gBoard to piece
                                   -- clear the from position
                                   let updatedBoard' = clearPiece updatedBoard from
                                   return updatedBoard'

{- |
Removes a specified piece from the board.
-}
clearPiece :: Board -> (Int,Int) -> Board
clearPiece gBoard pos = replace2 gBoard pos E

{- |
Collects a list of all the final destinations in a list
and then recursively call the whoWins method to find out
which player takes the win for that space on the gameboard.
-}
playerStack :: [Cell] -> Bool -> Cell
-- playerStack xs attack = foldl (\acc x -> (whoWins acc x attack)) E xs
playerStack xs attack = foldl (\acc x -> (whoWins acc x attack)) E xs

{- |
Determines on collision, which piece wins the clash and takes over the square on
the gameboard.
-}
whoWins :: Cell -> Cell -> Bool -> Cell
-- order matters when attack is false!!
-- pieceA is from
-- pieceB is to
whoWins pieceA pieceB collisionMode
    -- collision mode True
    -- when both knights collide
    | (pieceA == BK && pieceB == WK && collisionMode == True) = E
    -- when both pawns collide
    | (pieceA == BP && pieceB == WP && collisionMode == True) = E
    -- when a white knight collides with black pawn or empty space
    | (((pieceA == WK && (pieceB == BP || pieceB == E)) || ((pieceB == BP || pieceB == E) && pieceB == WK)) && collisionMode == True) = WK
    -- when a black knight collides with white pawn or empty space
    | (((pieceA == BK && (pieceB == WP || pieceB == E)) || ((pieceB == WP || pieceB == E) && pieceB == BK)) && collisionMode == True) = BK
    -- collision mode False (order is important!!)
    | (pieceA == BK && collisionMode == False) = BK
    | (pieceA == WK && collisionMode == False) = WK
    | (pieceA == WP && collisionMode == False) = WP
    | (pieceA == BP && collisionMode == False) = BP
    | (pieceA == E && pieceB /= E) = pieceB
    | otherwise = E

--ENDGAME
{- |
Determines which end game scenario has occurred and calls methods to output
this determination to the user.
-}
endTheGame :: Int -> Int -> Int -> Int -> IO ()
endTheGame blackpen whitepen blackPawnsLeft whitePawnsLeft
    | (blackPawnsLeft == 0) = endGameScene White
    | (whitePawnsLeft == 0) = endGameScene Black
    | (blackpen > whitepen) = endGameScene White
    | (blackpen < whitepen) = endGameScene Black
    | (blackpen == whitepen) = endGameDraw
    | otherwise = endGameDraw

{- |
Displays the winner of the game to the console.
-}
endGameScene :: Player -> IO ()
endGameScene winner = putStrLn (show(winner) ++ " takes the game!\n\n << Game Over >>")

{- |
Displays the end game message in the case that the game is a draw.
-}
endGameDraw :: IO ()
endGameDraw = putStrLn ("The game is a draw!\n\n << Game Over >>")
