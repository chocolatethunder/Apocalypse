{- |
Module      : Main
Description : Template to get you started on the CPSC 449 Winter 2016 Apocalypse assignment.
Copyright   : Copyright 2016, Rob Kremer (rkremer@ucalgary.ca), University of Calgary.
License     : Permission to use, copy, modify, distribute and sell this software
              and its documentation for any purpose is hereby granted without fee, provided
              that the above copyright notice appear in all copies and that both that
              copyright notice and this permission notice appear in supporting
              documentation. The University of Calgary makes no representations about the
              suitability of this software for any purpose. It is provided "as is" without
              express or implied warranty.
Maintainer  : rkremer@ucalgary.ca
Stability   : experimental
Portability : ghc 7.10.2 - 7.10.3

This module is used for CPSC 449 for the Apocalypse assignment.

Feel free to modify this file as you see fit.

-}

module Main (
      -- * Main
      main, main',
      -- * Utility functions
      replace, replace2
      ) where

import Data.Maybe (fromJust, isNothing)
import System.Environment
import System.IO.Unsafe
import ApocTools
import ApocStrategyHuman
import System.Exit
--import ApohcAIGreedy
--import ApochAIRandom

---Main-------------------------------------------------------------

-- | The main entry, which just calls 'main'' with the command line arguments.
main = main' (unsafePerformIO getArgs)
legalStategies = ["human", "greedy"] -- strategies list

{- | We have a main' IO function so that we can either:

     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}
main'           :: [String] -> IO()
main' args = do
    args <- getArgs
    case args of
               [] -> interactiveMode
               (x:xs) -> if (length args == 2) then checkLegalStrategy args else illegalStrategies
    
    putStrLn "\nThe initial board:"
    print initBoard
                                                                                                                                                     
    fst_input <- parse_input
    snd_input <- parse_input
    let temp = save_game fst_input snd_input initBoard
    show_game temp
                                         
-- change the game state and return it
save_game move_1 move_2 curr_board = GameState (check_move move_1) (blackPen curr_board) (check_move move_2) (whitePen curr_board)
                                                                   (replace2 (replace2 (replace2 (replace2 (theBoard curr_board) ((fromJust move_1) !! 1) (getFromBoard (theBoard curr_board) ((fromJust move_1) !! 0)))
                                                                        ((fromJust move_1) !! 0) E) ((fromJust move_2) !! 1) (getFromBoard (theBoard curr_board) ((fromJust move_2) !! 0)))
                                                                          ((fromJust move_2) !! 0) E)
-- Print the current game state and go to function check_game_status
show_game g_board = do
                        putStrLn (show $ g_board)
                        check_game_status g_board

-- Check the current game state and decide if the game should end or continue
check_game_status cur_board = do bp <- parse_input
                                 wp <- parse_input
                                 if (1 == 1) then show_game (save_game bp wp cur_board) else return()
                      
-- Check and see if the input is valid
check_move st = if (st==Nothing) then Passed else do Played (head (fromJust st), head (tail (fromJust st)))

-- interactive mode
interactiveMode :: IO ()
interactiveMode = do 
        putStrLn "\nPossible strategies:\n  human\n  greedy"
        putStrLn "Enter the strategy for BLACK:"
        blackStrategy <- getLine
        putStrLn blackStrategy
        if (blackStrategy `elem` legalStategies) then putStrLn " assign to black player" else illegalStrategies
        putStrLn "Enter the strategy for WHITE:"
        whiteStrategy <- getLine
        putStrLn whiteStrategy
        if (whiteStrategy `elem` legalStategies) then putStrLn " assign to black player" else illegalStrategies
        

-- Checks that strategies are legal
checkLegalStrategy :: [String] -> IO ()
checkLegalStrategy list =  if (((head list :: String) `elem` legalStategies) && ((last list :: String) `elem` legalStategies) )
                                         then putStrLn "Good strategies" else illegalStrategies

-- end game if illegal
illegalStrategies :: IO a
illegalStrategies = do
         putStrLn "\nPossible strategies:\n  human\n  greedy\n\nGAME OVER"
         exitFailure

         
-- get user input NOT COMPLETE
promptInput :: String -> IO String
promptInput prompt = do 
            putStrLn prompt
            getLine
            
--parse the input from command line
parse_input = do
                  input <- promptInput "get input\n"
                  let b = take 4 (words input)
                  let x_from = read (b !! 0) :: Int
                  let y_from = read (b !! 1) :: Int
                  let x_to = read (b !! 2) :: Int
                  let y_to = read (b !! 3) :: Int
                  return (Just [(x_from,y_from),(x_to,y_to)])
                  
                  
                  
                  
---2D list utility functions-------------------------------------------------------

-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                     in (if null zs then (if null ys then [] else init ys) else ys)
                        ++ [elem]
                        ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)

