-- This module is just a giant loop. This is the meat of the whole thing. 
-- It runs the game until the endgame scenario is reach. It check and categorizes
-- each move made by the user/computer then updates the game state. 

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
                                                -- incoming data is a Maybe [(Int,Int)]
                                                blackMove <- (getPlayerMove currBoard bl playType Black) -- Raw incoming data
                                                whiteMove <- (getPlayerMove currBoard wt playType White) -- Raw incoming data
                                                
                                                -- send it to be parsed then pushed into 
                                                -- the current game state. Need to pass p1 p2
                                                -- so that we know what type of players are 
                                                -- playing when looping back to the beginning
                                                
                                                -- putStrLn (show(getFromBoard (theBoard currBoard) ((fromJust p1move) !! 0)))              
                                                
                                                --let blackPlay = case blackMove of Nothing -> Passed :: Played
                                                --                                  xs -> Played ((fromJust xs)!!0,(fromJust xs)!!1)
                                                --let whitePlay = case whiteMove of Nothing -> Passed :: Played
                                                --                                  xs -> Played ((fromJust xs)!!0,(fromJust xs)!!1)
                                                

                                                -- Determine if it is a Nothing, Length 1 List, Length 2 List
                                                -- if Nothing then set played = Passed
                                                -- 
                                                -- if Length 1 then check if the player is trying to upgrade to knight
                                                    -- get the piece at the coordinate 
                                                    -- if 
                                                        -- it is a pawn
                                                        -- it is at (y=4(black) or y=0(white)) 
                                                        -- numKnights is <2 then upgrade
                                                        -- dest is empty
                                                        -- then upgrade pawn
                                                    -- 
                                                
                                                --      Both players pass on the same round. The one with the most pawns wins.
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
                                                        isBlackMoveValid <- validateMove currBoard blackMove Black
                                                        isWhiteMoveValid <- validateMove currBoard whiteMove White

                                                        let newBlackPenalty = case isBlackMoveValid of False -> succ blackPenalty
                                                                                                       True -> blackPenalty
                                                        let blackPlayed = case isBlackMoveValid of False -> Goofed (getTwoCoords blackMove)
                                                                                                   True -> Played (getTwoCoords blackMove)
                                                        


                                                        let whitePlayed = case isWhiteMoveValid of False -> Goofed (getTwoCoords whiteMove)
                                                                                                   True -> Played (getTwoCoords whiteMove)
                                                        let newWhitePenalty = case isWhiteMoveValid of False -> succ whitePenalty
                                                                                                       True -> whitePenalty
                                                        

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

                                                        -- Update the board here
                                                        let updateBoard = replace2 (theBoard currBoard) (snd(getTwoCoords whiteMove)) (getFromBoard (theBoard currBoard) (fst(getTwoCoords whiteMove)))
                                                        let updateBoard' = replace2 (updateBoard) (fst(getTwoCoords whiteMove)) E
                                                        let updateBoard'' = replace2 (updateBoard') (snd(getTwoCoords blackMove)) (getFromBoard (theBoard currBoard) (fst(getTwoCoords blackMove)))
                                                        let updateBoard''' = replace2 (updateBoard'') (fst(getTwoCoords blackMove)) E
                                                        
                                                        -- Save game state here
                                                        let newBoard = GameState (blackPlayed)
                                                                                  (newBlackPenalty)
                                                                                  (whitePlayed)
                                                                                  (newWhitePenalty)
                                                                                  (updateBoard''')
                                                        -- Loop back
                                                        putStrLn (show (newBoard))
                                                        gameLoop newBoard bl wt Normal False 

                                                        
                                                {- --------- CONSTRUCTION ENDS ------------- -}
                                        
                                else
                                    do 
                                        -- this does NOT account for a tie yet
                                        if (blackPen currBoard >= 2 || arePawnsLeft (theBoard currBoard) Black) then 
                                            endGameScene White
                                        else 
                                            endGameScene Black
                                return ()



 
-- this determines what type of playertype is playing
-- WARNING!! This is NOT dynamic. If the strats change
-- please update this function accordingly.
getPlayerMove :: GameState -> [Char] -> PlayType -> Player -> IO (Maybe [(Int,Int)])
-- currBoard: current Gamestate data
-- playerType: type of player/strat coming in
-- playType: Normal or PawnPlacement
-- currPlayer: Black or White player making the play
getPlayerMove currBoard playerType playType currPlayer
    | playerType == "Human" = humanPlayer currBoard playType currPlayer
    | playerType == "Computer" = aiMove currBoard playType currPlayer "offensive"
    | playerType == "Random" = aiMove currBoard playType currPlayer "random"
    | otherwise = return (Just [(0,0)]) -- this needs to error out. To do

-- This function will pass the game board to the
-- appropriate AI type and get a Maybe [(Int,Int)] of 
-- correctly formatted coordinates. (Work in progress)
aiMove :: GameState -> PlayType -> Player -> [Char] -> IO (Maybe [(Int,Int)])
aiMove currBoard playType playerType aiType 
    | aiType == "offensive" = aiOffensive currBoard playType playerType
    | aiType == "random" = aiRandom currBoard playType playerType
    | otherwise = return(Just [(0,0)]) -- this needs to error out. To do



-- endgame
endGameScene :: Player -> IO ()
endGameScene winner = putStrLn (show(winner) ++ " takes the game!\n\n << Game Over >>")
