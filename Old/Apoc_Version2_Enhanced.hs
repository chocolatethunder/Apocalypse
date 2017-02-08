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
                                           playRound blackStrategy whiteStrategy initBoard  -- plays first round
                                       else illegalStrategies -- ends game
        

-- Checks that strategies are legal
checkLegalStrategy :: [String] -> IO ()
checkLegalStrategy list =  if (((head list :: String) `elem` legalStategies) && ((last list :: String) `elem` legalStategies) )
                                         then do
                                             putStrLn "\nThe initial board:"
                                             --print initBoard  -- prints initial board
                                             playRound (head list :: String) (last list :: String) initBoard  -- plays first round
                                         else illegalStrategies -- ends game


                                         
-- play rounds
-- checks boolean, if true then round continues, if false, game over
playRound :: String -> String -> GameState -> IO ()
playRound blackStrategy whiteStrategy old_gamestate = do
                                                 putStrLn (show old_gamestate)
                                                 check_end old_gamestate
                                                 new_gamestate <- (playerMoves blackStrategy whiteStrategy old_gamestate)
                                                 playRound blackStrategy whiteStrategy new_gamestate
                               
-- checks the game status, should return false if
--              - penalty
--              - no more pawns
--              - both players pass on the same round

{-data GameState = GameState { blackPlay :: Played  -- ^ The black player's play type
                           , blackPen  :: Int     -- ^ The black player's penalty
                           , whitePlay :: Played  -- ^ The white player's play type
                           , whitePen  :: Int     -- ^ The white player's penalty
                           , theBoard  :: Board   -- ^ The actual board.
                           }-}

check_end :: GameState -> IO ()
check_end curr_game_status
                       | ((blackPen curr_game_status) >= 2)        = do putStrLn "Game Over! White wins!"
                                                                        exitFailure
                       | ((whitePen curr_game_status) >= 2)        = do putStrLn "Game Over! Black wins!"
                                                                        exitFailure
                       | ((count_chess curr_game_status '/') == 0) = do putStrLn "Game Over! White wins!"
                                                                        exitFailure
                       | ((count_chess curr_game_status '+') == 0) = do putStrLn "Game Over! Black wins!"
                                                                        exitFailure
                       | otherwise                                 = return ()

count_chess :: GameState -> Char -> Int
count_chess game_status color = sum [ sum [ 1 | y <- x, (cell2Char y) == color ] | x <- (theBoard game_status)]

-- makes the player move and returns the updated gamestate
-- should check for type of move and whether it is legal or not
playerMoves :: String -> String -> GameState -> IO GameState
playerMoves blackStrategy whiteStrategy curr_gamestate
                          | (blackStrategy == "human" && whiteStrategy == "human")  = do
                                                                                        -- need to add prompt message
                                                                                        putStrLn "Please enter input for Black:"
                                                                                        blackMove <- getLine
                                                                                        putStrLn "Please enter input for White:"
                                                                                        whiteMove <- getLine
                                                                                        let gs = GameState (check_ept (parse_input blackMove) curr_gamestate)
                                                                                                  (check_Pen (blackPen curr_gamestate) (check_ept_bool (parse_input blackMove) curr_gamestate))
                                                                                                  (check_ept (parse_input whiteMove) curr_gamestate)
                                                                                                  (check_Pen (whitePen curr_gamestate) (check_ept_bool (parse_input whiteMove) curr_gamestate))
                                                                                                  (check_valid_replace (parse_input whiteMove) (check_valid_replace (parse_input blackMove) (theBoard curr_gamestate) (check_ept_bool (parse_input blackMove) curr_gamestate)) (check_ept_bool (parse_input whiteMove) curr_gamestate))
                                                                                        return gs
               
                                         
                      
-- Check and see if the input is valid

check_Pen :: Int -> Bool -> Int
check_Pen current_Pen b = if (not b) then current_Pen + 1 else current_Pen

-- return bool type -- start --
check_ept_bool :: Maybe [(Int, Int)] -> GameState -> Bool
check_ept_bool st temp = if (null st) then True else check_legal_move_bool st (show (getFromBoard (theBoard temp) ((fromJust st) !! 0))) (show (getFromBoard (theBoard temp) ((fromJust st) !! 1)))

check_legal_move_bool :: Maybe [(Int, Int)] -> [Char] -> [Char] -> Bool
check_legal_move_bool one two three = if (two == "X" || two == "#") then check_legal_move_knight_bool one else check_legal_move_pawn_bool one two three

check_legal_move_knight_bool :: Maybe [(Int, Int)] -> Bool
check_legal_move_knight_bool one = do
                                 if ((temp_x == 2 && temp_y == 1) || (temp_x == 1 && temp_y == 2))
                                 then True
                                 else False
                                 where temp_x = if (fst (head (fromJust one)) > fst (head (tail (fromJust one)))) then fst (head (fromJust one)) - fst (head (tail (fromJust one))) else fst (head (tail (fromJust one))) - fst (head (fromJust one))
                                       temp_y = if (snd (head (fromJust one)) > snd (head (tail (fromJust one)))) then snd (head (fromJust one)) - snd (head (tail (fromJust one))) else snd (head (tail (fromJust one))) - snd (head (fromJust one))

check_legal_move_pawn_bool :: Maybe [(Int, Int)] -> [Char] -> [Char] -> Bool                   
check_legal_move_pawn_bool one two dst
                          | (temp_y == 1 && temp_x == 0 && two == "/" && dst == "_")                                    = True
                          | (temp_y == -1 && temp_x == 0 && two == "+" && dst == "_")                                   = True
                          | (temp_y == 1 && (temp_x == -1 || temp_x == 1) && two == "/" && (dst == "+" || dst == "#"))  = True
                          | (temp_y == -1 && (temp_x == -1 || temp_x == 1) && two == "+" && (dst == "/" || dst == "X")) = True
                          | otherwise                                                                                   = False
                            where temp_x = fst (head (tail (fromJust one))) - fst (head (fromJust one))
                                  temp_y = snd (head (tail (fromJust one))) - snd (head (fromJust one))
-- return bool type -- end --

-- return play type -- start --
check_ept :: Maybe [(Int, Int)] -> GameState -> Played
check_ept st temp = if (null st) then Passed else check_legal_move st (show (getFromBoard (theBoard temp) ((fromJust st) !! 0))) (show (getFromBoard (theBoard temp) ((fromJust st) !! 1))) temp

check_legal_move :: Maybe [(Int, Int)] -> [Char] -> [Char] -> GameState -> Played
check_legal_move one two three four = if (two == "X" || two == "#") then check_legal_move_knight one else check_legal_move_pawn one two three four

check_legal_move_knight :: Maybe [(Int, Int)] -> Played
check_legal_move_knight one = do
                                 if ((temp_x == 2 && temp_y == 1) || (temp_x == 1 && temp_y == 2))
                                 then valid_move one
                                 else Goofed (head (fromJust one), head (tail (fromJust one)))
                                 where temp_x = if (fst (head (fromJust one)) > fst (head (tail (fromJust one)))) then fst (head (fromJust one)) - fst (head (tail (fromJust one))) else fst (head (tail (fromJust one))) - fst (head (fromJust one))
                                       temp_y = if (snd (head (fromJust one)) > snd (head (tail (fromJust one)))) then snd (head (fromJust one)) - snd (head (tail (fromJust one))) else snd (head (tail (fromJust one))) - snd (head (fromJust one))

check_legal_move_pawn :: Maybe [(Int, Int)] -> [Char] -> [Char] -> GameState -> Played
check_legal_move_pawn one two dst stat
                          | (temp_y == 1 && temp_x == 0 && two == "/" && dst == "_")                                    = if ((snd (head (tail (fromJust one)))) == 4) then can_upgrade stat '/' 'X' one else valid_move one
                          | (temp_y == -1 && temp_x == 0 && two == "+" && dst == "_")                                   = if ((snd (head (tail (fromJust one)))) == 0) then can_upgrade stat '+' '#' one else valid_move one
                          | (temp_y == 1 && (temp_x == -1 || temp_x == 1) && two == "/" && (dst == "+" || dst == "#"))  = if ((snd (head (tail (fromJust one)))) == 4) then can_upgrade stat '/' 'X' one else valid_move one
                          | (temp_y == -1 && (temp_x == -1 || temp_x == 1) && two == "+" && (dst == "/" || dst == "X")) = if ((snd (head (tail (fromJust one)))) == 0) then can_upgrade stat '+' '#' one else valid_move one
                          | otherwise                                                                                   = Goofed (head (fromJust one), head (tail (fromJust one)))
                            where temp_x = fst (head (tail (fromJust one))) - fst (head (fromJust one))
                                  temp_y = snd (head (tail (fromJust one))) - snd (head (fromJust one))

can_upgrade :: GameState -> Char -> Char -> Maybe [(Int, Int)] -> Played
can_upgrade stat sym k move = if (((count_chess stat k) >= 3) && ((count_chess stat sym) == 1)) then pawn_placement move else pawn_upgrade move

pawn_upgrade :: Maybe [(Int, Int)] -> Played
pawn_upgrade move = UpgradedPawn2Knight (head (tail (fromJust move)))

pawn_placement :: Maybe [(Int, Int)] -> Played
pawn_placement move = PlacedPawn (head (fromJust move), (2, 2))

-- return play type -- end --

valid_move :: Maybe [(Int, Int)] -> Played
valid_move move = Played (head (fromJust move), head (tail (fromJust move)))
                             
type_convert :: GameState -> Board -> GameState
type_convert game board = GameState (blackPlay game) (blackPen game) (whitePlay game) (whitePen game) (board)
              
valid_replace :: Maybe [(Int, Int)] -> Board -> Board
valid_replace mv bd = if ((length mv) == 1) then replace2 (replace2 bd ((fromJust mv) !! 1) (getFromBoard bd ((fromJust mv) !! 0))) ((fromJust mv) !! 0) E else bd

check_valid_replace :: Maybe [(Int, Int)] -> Board -> Bool -> Board
check_valid_replace mv bd b = if (b) then (valid_replace mv bd) else bd

-- ends game should tell us who the winner is
endGame :: GameState -> IO ()
endGame curr_gamestate = putStrLn "game over"


-- end game if illegal
illegalStrategies :: IO a
illegalStrategies = do
         putStrLn "\nPossible strategies:\n  human\n  greedy\n\nGAME OVER"
         exitFailure
      
--parse the input from command line
parse_input :: String -> Maybe [(Int, Int)]
parse_input input | length (words input) == 4 = if (x_from < 5 && y_from < 5 && x_to < 5 && y_to < 5) then Just [(x_from,y_from),(x_to,y_to)] else Nothing
                  | otherwise                 = Nothing
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

