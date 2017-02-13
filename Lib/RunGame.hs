{- |
   This module is just a giant loop. This is the meat of the whole thing.
   It runs the game until the endgame scenario is reach. It check and categorizes
   each move made by the user/computer then updates the game state.
-}
module Lib.RunGame (gameLoop) where

import Data.Maybe
import Control.Monad

import ApocTools
import Lib.Language
import Lib.Functions
import AI.ApocStrategyHuman
import AI.Offensive
import AI.Random


-- Use game loop with the current game state and whether we
-- have determined whether the game should end or not. All gl_ functions
-- are relevent to the gameloop flow.
gameLoop :: GameState -> [Char] -> [Char] -> PlayType -> Bool -> IO()
gameLoop currBoard bl wt playType endGame = do
                                if (endGame == False) then
                                    do

                                        {- --------- AREA IS UNDER CONSTRUCTION ------------- -}

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
                                                        
                                                        -- Check if next round is a "Normal" round or a "PawnPlacement" round                                                        
                                                        
                                                        let checkBlackUpgrade = checkPawnUpgrade updatedBoard blackMove
                                                        let checkWhiteUpgrade = checkPawnUpgrade updatedBoard whiteMove

                                                        -- Save game state here
                                                        let newBoard = GameState (blackPlayed)
                                                                                  (newBlackPenalty)
                                                                                  (whitePlayed)
                                                                                  (newWhitePenalty)
                                                                                  (updatedBoard)
                                                        -- Loop back
                                                        putStrLn (show (newBoard))
                                                        gameLoop newBoard bl wt Normal False



                                                {- --------- CONSTRUCTION ENDS ------------- -}

                                else
                                    do
                                        -- this does NOT account for a tie yet
                                        if (blackPen currBoard >= 2 || arePawnsLeft (theBoard currBoard) White) then
                                            endGameScene White
                                        else
                                            endGameScene Black
                                return ()


--gameLoop currBoard bl wt playType endGame = do


    

-- this determines what type of playertype is playing
-- WARNING!! This is NOT dynamic. If the strats change
-- please update this function accordingly.
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

-- This function will pass the game board to the
-- appropriate AI type and get a Maybe [(Int,Int)] of
-- correctly formatted coordinates. (Work in progress)
aiMove :: GameState -> PlayType -> Player -> [Char] -> IO (Maybe [(Int,Int)])
aiMove currBoard playType playerType aiType
    | aiType == "offensive" = aiOffensive currBoard playType playerType
    | aiType == "random" = aiRandom currBoard playType playerType
    | otherwise = return(Just [(0,0)]) -- this needs to error out. To do


{- PAWNPLACEMENT MODE -}

checkPawnUpgrade :: Board -> Maybe [(Int,Int)] -> Bool
checkPawnUpgrade gBoard move 
    | (finalY == 4 && movingUnit == WP) = True
    | (finalY == 0 && movingUnit == BP) = True
    | otherwise = False
    where 
        finalY = snd(snd(getTwoCoords(move)))
        movingUnit = getFromBoard gBoard (snd(getTwoCoords(move)))
    
{- COLLISION DETECTION FUNCTIONS -}

-- this updates the game board based on players moves that are valid
updateGameBoard :: Board -> Bool -> Bool -> Maybe [(Int,Int)] -> Maybe [(Int,Int)] -> IO Board
updateGameBoard gBoard isBlackMoveValid isWhiteMoveValid blackMove whiteMove 
    | (isBlackMoveValid == True && isWhiteMoveValid == False) = collision gBoard blackMove Nothing
    | (isBlackMoveValid == False && isWhiteMoveValid == True) = collision gBoard Nothing whiteMove
    | (isBlackMoveValid == True && isWhiteMoveValid == True) = collision gBoard blackMove whiteMove
    | otherwise = collision gBoard Nothing Nothing

-- Main collision detection function
collision :: Board -> Maybe [(Int,Int)] -> Maybe [(Int,Int)] -> IO Board
collision gBoard bPos wPos 
    -- Both players move AND there IS collision between them
    | (bPos /= Nothing && wPos /= Nothing && playerCollision == True && playerSwap == False) = do  
                                                                            -- The black piece, the white piece, and the one that is already there compete
                                                                            let winner = playerStack [(getFromBoard gBoard bToPos),(getFromBoard gBoard bFromPos),(getFromBoard gBoard wFromPos)]
                                                                            -- move the black player. The player to move here is arbitary just as long as
                                                                            -- the opposite player has it;s original loction cleaned up
                                                                            newBoard <- movePlayer gBoard winner bFromPos bToPos
                                                                            -- clear the white player's original position
                                                                            let newBoard' = clearPiece newBoard wFromPos
                                                                            -- return the updated board
                                                                            return newBoard'
    -- Both players move WITHOUT any between them                                                                        
    | (bPos /= Nothing && wPos /= Nothing && playerCollision == False && playerSwap == False) = do                                                                             
                                                                            -- move the white piece first to account for chase condition
                                                                            let wwinner = playerStack [(getFromBoard gBoard wFromPos),(getFromBoard gBoard wToPos)]
                                                                            newBoard <- movePlayer gBoard wwinner wFromPos wToPos
                                                                            -- move the black piece
                                                                            let bwinner = playerStack [(getFromBoard newBoard bFromPos),(getFromBoard newBoard bToPos)]
                                                                            newBoard' <- movePlayer newBoard bwinner bFromPos bToPos
                                                                            -- return the updated board
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
                            let winner = playerStack [(getFromBoard gBoard wFromPos),(getFromBoard gBoard wToPos)]
                            newBoard <- movePlayer gBoard winner wFromPos wToPos
                            return newBoard
    -- only Black player moves
    | (wPos == Nothing) = do 
                            let winner = playerStack [(getFromBoard gBoard bFromPos),(getFromBoard gBoard bToPos)]
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

-- Moves a unit from a position to a position given a board
movePlayer :: Board -> Cell -> (Int,Int) -> (Int,Int) -> IO Board
movePlayer gBoard piece from to = do 
                                   -- update to position
                                   let updatedBoard = replace2 gBoard to piece
                                   -- clear the from position
                                   let updatedBoard' = clearPiece updatedBoard from
                                   return updatedBoard'

-- Replace a unit on the board with an Empty piece  
clearPiece :: Board -> (Int,Int) -> Board                         
clearPiece gBoard pos = replace2 gBoard pos E

-- collect a list of all the final destinations in a list 
-- and then recursively call the whoWins method to find out 
-- which player takes the ground.
playerStack :: [Cell] -> Cell
playerStack xs = foldl (\acc x -> (whoWins acc x)) E xs

-- This function determins who comes out on top during engagement
-- It contains hardcoded player engagement definition and the order
-- does not matter.
whoWins :: Cell -> Cell -> Cell
whoWins pieceA pieceB
    | (pieceA == BK && pieceB == WK) = E
    | (pieceA == BP && pieceB == WP) = E
    | ((pieceA == BK && (pieceB == E || pieceB == WP)) || ((pieceA == E || pieceA == WP) && pieceB == BK)) = BK
    | ((pieceA == WK && (pieceB == E || pieceB == BP)) || ((pieceA == E || pieceA == BP) && pieceB == WK)) = WK
    | ((pieceA == WP && pieceB == E) || (pieceA == E && pieceB == WP)) = WP
    | ((pieceA == BP && pieceB == E) || (pieceA == E && pieceB == BP)) = BP
    | otherwise = E


{- ENDGAME -}
endGameScene :: Player -> IO ()
endGameScene winner = putStrLn (show(winner) ++ " takes the game!\n\n << Game Over >>")
