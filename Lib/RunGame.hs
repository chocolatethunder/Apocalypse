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



                                                        -- Collision detection here

                                                        
                                                        -- Update the board here
                                                        
                                                        let updateBoard = replace2 (theBoard currBoard) (snd(getTwoCoords whiteMove)) (getFromBoard (theBoard currBoard) (fst(getTwoCoords whiteMove)))
                                                        let updateBoard' = replace2 (updateBoard) (fst(getTwoCoords whiteMove)) E
                                                        let updateBoard'' = replace2 (updateBoard') (snd(getTwoCoords blackMove)) (getFromBoard (theBoard currBoard) (fst(getTwoCoords blackMove)))
                                                        let updateBoard''' = replace2 (updateBoard'') (fst(getTwoCoords blackMove)) E
                                                        
                                                        -- Check if next round is a "Normal" round or a "PawnPlacement" round
                                                        
                                                        

                                                        -- Save game state here
                                                        let newBoard = GameState (blackPlayed)
                                                                                  (newBlackPenalty)
                                                                                  (whitePlayed)
                                                                                  (newWhitePenalty)
                                                                                  (updateBoard''')
                                                        -- Loop back
                                                        putStrLn (show (newBoard))
                                                        gameLoop newBoard bl wt Normal False
                                                        
                                                        
                                                        -- DEBUG
                                                        {-
                                                        putStrLn "\n---- DEBUG ----"

                                                        putStrLn (show (newBlackPenalty))
                                                        putStrLn (show (newWhitePenalty))

                                                        putStrLn (show (isBlackMoveValid))
                                                        putStrLn (show (isWhiteMoveValid))

                                                        putStrLn (show (getPawnsLeft (theBoard currBoard) Black))
                                                        putStrLn (show (getPawnsLeft (theBoard currBoard) White))

                                                        putStrLn "---- DEBUG ----\n"
                                                        -}


                                                {- --------- CONSTRUCTION ENDS ------------- -}

                                else
                                    do
                                        -- this does NOT account for a tie yet
                                        if (blackPen currBoard >= 2 || arePawnsLeft (theBoard currBoard) White) then
                                            endGameScene White
                                        else
                                            endGameScene Black
                                return ()




-- this determines what type of playertype is playing
-- WARNING!! This is NOT dynamic. If the strats change
-- please update this function accordingly.
getPlayerMove :: GameState -> [Char] -> PlayType -> Player -> IO (Maybe [(Int,Int)])
-- currBoard: current Gamestate data
-- stratType: type of player/strat coming in
-- playType: Normal or PawnPlacement
-- playerType: Black or White player making the play
getPlayerMove currBoard stratType playType playerType
    | stratType == "Human" = humanPlayer currBoard playType playerType
    | stratType == "Computer" = aiMove currBoard playType playerType "offensive"
    | stratType == "Random" = aiMove currBoard playType playerType "random"
    | otherwise = return (Just [(0,0)]) -- this needs to error out. To do

-- This function will pass the game board to the
-- appropriate AI type and get a Maybe [(Int,Int)] of
-- correctly formatted coordinates. (Work in progress)
aiMove :: GameState -> PlayType -> Player -> [Char] -> IO (Maybe [(Int,Int)])
aiMove currBoard playType playerType aiType
    | aiType == "offensive" = aiOffensive currBoard playType playerType
    | aiType == "random" = aiRandom currBoard playType playerType
    | otherwise = return(Just [(0,0)]) -- this needs to error out. To do


-- Collision Detection Functions

-- collect a list of all the final destinations in a list 
-- and then recursively call the whoWins method to find out 
-- which player takes the ground. 


-- This function determins who comes out on top during engagement
whoWins :: Cell -> Cell -> Cell
-- bPlayer: Black Player
-- wPlayer: White Player
-- Note Order is important!
whoWins bPlayer wPlayer
    | (bPlayer == BK && wPlayer == WK) = E
    | (bPlayer == BP && wPlayer == WP) = E
    | (bPlayer == BK && (wPlayer == E || wPlayer == WP)) = BK
    | ((bPlayer == E || bPlayer == BP) && wPlayer == WK) = WK
    | otherwise = E


-- endgame
endGameScene :: Player -> IO ()
endGameScene winner = putStrLn (show(winner) ++ " takes the game!\n\n << Game Over >>")
