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



-- interactive mode
interactiveMode :: IO ()
interactiveMode = do 
        putStrLn "\nPossible strategies:\n  human\n  greedy"
        putStrLn "Enter the strategy for BLACK:"
        blackStrategy <- getLine
        putStrLn blackStrategy
        putStrLn "Enter the strategy for WHITE:"
        whiteStrategy <- getLine
        putStrLn whiteStrategy
        if (blackStrategy `elem` legalStategies && whiteStrategy `elem` legalStategies) 
                                       then do
                                           putStrLn "\nThe initial board:"
                                           --print initBoard  -- prints initial board
                                           playRound blackStrategy whiteStrategy initBoard (checkGameStatus initBoard)  -- plays first round
                                       else illegalStrategies -- ends game
        

-- Checks that strategies are legal
checkLegalStrategy :: [String] -> IO ()
checkLegalStrategy list =  if (((head list :: String) `elem` legalStategies) && ((last list :: String) `elem` legalStategies) )
                                         then do
                                             putStrLn "\nThe initial board:"
                                             --print initBoard  -- prints initial board
                                             playRound (head list :: String) (last list :: String) initBoard (checkGameStatus initBoard) -- plays first round
                                         else illegalStrategies -- ends game


                                         
-- play rounds
-- checks boolean, if true then round continues, if false, game over
playRound :: String -> String -> GameState -> Bool -> IO ()
playRound blackStrategy whiteStrategy old_gamestate bool
                               | bool == True = do
                                                 putStrLn (show old_gamestate )
                                                 new_gamestate <- (playerMoves blackStrategy whiteStrategy old_gamestate)
                                                 playRound blackStrategy whiteStrategy new_gamestate (checkGameStatus new_gamestate)                                                 
                               | bool == False = endGame old_gamestate
                               
-- checks the game status, should return false if
--              - penalty
--              - no more pawns
--              - both players pass on the same round
checkGameStatus :: GameState -> Bool
checkGameStatus currentState = True

-- makes the player move and returns the updated gamestate
-- should check for type of move and whether it is legal or not
playerMoves :: String -> String -> GameState -> IO GameState
playerMoves blackStrategy whiteStrategy curr_gamestate
                          | (blackStrategy == "human" && whiteStrategy == "human")  = do
                                                                                        blackMove <- getLine
                                                                                        whiteMove <- getLine
                                                                                        let gs = GameState (check_ept (parse_input blackMove) curr_gamestate)
                                                                                                  (blackPen curr_gamestate)
                                                                                                  (check_ept (parse_input whiteMove) curr_gamestate)
                                                                                                  (whitePen curr_gamestate)
                                                                                                  (valid_replace (parse_input whiteMove) (type_convert (valid_replace (parse_input blackMove) curr_gamestate)))
                                                                                        return gs
               
                                         
                      
-- Check and see if the input is valid
check_ept :: Maybe [(Int, Int)] -> GameState -> Played
check_ept st temp = if (st==Nothing) then Passed else check_legal_move st (show (getFromBoard (theBoard temp) ((fromJust st) !! 0)))

check_legal_move :: Maybe [(Int, Int)] -> [Char] -> Played
check_legal_move one two = if (two == "X" || two == "#") then check_legal_move_knight one else check_legal_move_pawn one

check_legal_move_knight :: Maybe [(Int, Int)] -> Played
check_legal_move_knight one = do
                                 if ((temp_x == 2 && temp_y == 1) || (temp_x == 1 && temp_y == 2))
                                 then valid_move one
                                 else Passed
                                 where temp_x = if (fst (head (fromJust one)) > fst (head (tail (fromJust one)))) then fst (head (fromJust one)) - fst (head (tail (fromJust one))) else fst (head (tail (fromJust one))) - fst (head (fromJust one))
                                       temp_y = if (snd (head (fromJust one)) > snd (head (tail (fromJust one)))) then snd (head (fromJust one)) - snd (head (tail (fromJust one))) else snd (head (tail (fromJust one))) - snd (head (fromJust one))
                                
valid_move :: Maybe [(Int, Int)] -> Played
valid_move move = Played (head (fromJust move), head (tail (fromJust move)))

check_legal_move_pawn :: Maybe [(Int, Int)] -> Played
check_legal_move_pawn one = do
                                if ((temp_y == 1 || temp_y == -1) && temp_x == 0) then valid_move one else Passed
                                where temp_x = fst (head (tail (fromJust one))) - fst (head (fromJust one))
                                      temp_y = snd (head (tail (fromJust one))) - snd (head (fromJust one))
                             
type_convert :: Board -> GameState
type_convert abc = GameState Init 0 Init 0 (abc)
              
valid_replace :: Maybe [(Int, Int)] -> GameState -> Board
valid_replace mv bd = replace2 (replace2 (theBoard bd) ((fromJust mv) !! 1) (getFromBoard (theBoard bd) ((fromJust mv) !! 0))) ((fromJust mv) !! 0) E


-- ends game should tell us who the winner is
endGame :: GameState -> IO ()
endGame curr_gamestate = putStrLn "game over"


-- end game if illegal
illegalStrategies :: IO a
illegalStrategies = do
         putStrLn "\nPossible strategies:\n  human\n  greedy\n\nGAME OVER"
         exitFailure

         
-- get user input
promptInput :: String -> IO String
promptInput prompt = do 
            putStrLn prompt
            getLine
            
--parse the input from command line
parse_input :: String -> Maybe [(Int, Int)]
parse_input input = do
                       Just [(x_from,y_from),(x_to,y_to)]
                       where b = take 4 (words input)
                             x_from = read (b !! 0) :: Int
                             y_from = read (b !! 1) :: Int
                             x_to = read (b !! 2) :: Int
                             y_to = read (b !! 3) :: Int
                  
                  
                  
                  
---2D list utility functions-------------------------------------------------------
-- Needs collision detection
-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                     in (if null zs then (if null ys then [] else init ys) else ys)
                        ++ [elem]
                        ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)

