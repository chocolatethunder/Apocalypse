{-|
Module      : GameEngine
Description : CPSC449 W2017 Haskell Apocalypse Assignment
Copyright   : Kowther Hassan, Kaylee Stelter, Matthew Mullins, Saurabh Tomar, Tsz Lam
License     : None
Portability : ghc 7.10.2-3
-}


{- |
Retrieves the strategies from user input and then moves on to start and run the game.
-}
module GameEngine (checkStartMode) where

import ApocTools
import Language
import Functions
import RunGame

-- Game strategies
strats = ["human", "computer", "random"]

{- |
Checks the initial user input for length compatibility. If no input is detected
the game transitions to an interactive mode. If input of length 2 is detected, the
strategies chosen are checked. Otherwise an error is thrown.
-}
checkStartMode :: [[Char]] -> IO ()
checkStartMode strats
        | lengthOfArgs == 0 = interactiveMode
        | lengthOfArgs == 2 = checkStrat (head strats) (last strats)
        | otherwise         = stratNumInputError
        where lengthOfArgs = length strats

{- |
Checks the strategies retrieved from user input to see if they are valid.
The game begins with those strategies if they are valid, and an error is thrown
otherwise.
-}
checkStrat :: [Char] -> [Char] -> IO ()
checkStrat player1Strat player2Strat =
                                if ((player1Strat `elem` strats) && (player2Strat `elem` strats))
                                    -- start game with the two strategies
                                    then startGame player1Strat player2Strat
                                    -- fail and quit
                                    else stratInputError

{- |
Prints the error messages to the console if the length of the input is invalid
-}
stratNumInputError :: IO ()
stratNumInputError = do
                    putStrLn wrongStratNumMsg
                    printStrats strats
                    putStrLn gameOverMsg

{- |
Prints the error messages to the console if the strategies entered are invalid
-}
stratInputError :: IO ()
stratInputError = do
                    putStrLn stratErrorMsg
                    printStrats strats
                    putStrLn gameOverMsg

{- |
Interactive mode which displays welcome messages to the user and asks for the
strategies for each player. This input is then checked for validity.
-}
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

{- |
Uses the previously validated game strategies to start gameplay.
-}
startGame :: [Char] -> [Char] -> IO ()
startGame player1 player2 = do
                                -- Display start game message
                                putStrLn startGameMsg
                                -- print the initial board
                                print initBoard
                                -- Go into the game loop
                                gameLoop initBoard player1 player2 Normal False -- (goes to -----> RunGame Module)
                                return ()
