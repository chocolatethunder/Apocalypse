{- |
-- This module takes the strategies from the player and punches them into
-- the game and then moves on to start and then run the game.
-}

module Lib.GameEngine (checkStartMode) where

import ApocTools
import Lib.Language
import Lib.Functions
import Lib.RunGame

{- SHARED DATA -}
-- Allowes strategies/modes
strats = ["Human", "Computer", "Random"]

{- FUNCTIONS -}
-- This function makes sure that there are only 0 or 2 inputs that
-- are allowed to go through. Error checking built in.
checkStartMode :: [[Char]] -> IO ()
checkStartMode strats
        | lengthOfArgs == 0 = interactiveMode
        | lengthOfArgs == 2 = checkStrat (head strats) (last strats)
        | otherwise         = stratNumInputError
        where lengthOfArgs = length strats

-- Process two argument style only
checkStrat :: [Char] -> [Char] -> IO ()
checkStrat player1Strat player2Strat =
                                if ((player1Strat `elem` strats) && (player2Strat `elem` strats))
                                    -- start game with the two strategies
                                    then startGame player1Strat player2Strat
                                    -- fail and quit
                                    else stratInputError

-- handle strategy input error
stratNumInputError :: IO ()
stratNumInputError = do
                    putStrLn wrongStratNumMsg
                    printStrats strats
                    putStrLn gameOverMsg

stratInputError :: IO ()
stratInputError = do
                    putStrLn stratErrorMsg
                    printStrats strats
                    putStrLn gameOverMsg

-- engage Interactive Mode to capture the 2 player strategies
interactiveMode :: IO ()
interactiveMode = do
                    putStrLn welcomeMsg
                    printStrats strats
                    -- get black player strat
                    putStrLn blackStrat
                    blackSratType <- getLine
                    -- get white player strat
                    putStrLn whiteStrat
                    whiteSratType <- getLine
                    -- send it for checking
                    checkStrat blackSratType whiteSratType

-- start the game by calling the game gameLoop function
-- this function is protected by the error checking
-- from the previous functions before it.
startGame :: [Char] -> [Char] -> IO ()
startGame player1 player2 = do
                                -- Display start game message
                                putStrLn startGameMsg
                                -- print the initial board
                                print initBoard
                                -- Go into the game loop
                                gameLoop initBoard player1 player2 Normal False -- (goes to -----> RunGame Module)
                                return ()
