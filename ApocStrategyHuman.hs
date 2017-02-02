{- |
Module      : ApocStrategyHuman
Description : Template for a game-playing strategy definition.
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

This is merely a skeleton to get you started on creating a strategy for playing the
Apocalypse game.  It has VERY little functionality.
-}

module ApocStrategyHuman (
   human
   ) where

import ApocTools
import System.Environment

{- | This is just a placeholder for the human strategy: it always chooses to play
     (0,0) to (2,1).
-}

-- get user input NOT COMPLETE
promptInput :: String -> IO String
promptInput prompt = do 
            putStr prompt
            getLine
       
       
       
        
-- checks if user input is between 0 and 4 inclusing. Need to add more checks here
checkInput :: String -> Bool
checkInput list = and [
              length list >= 0
            , length list <= 4
            ]

--parse the input from command line
parse_input = do
                  input <- promptInput "get input\n"
                  let b = take 4 (words input)
                  let x_from = read (b !! 0) :: Int
                  let y_from = read (b !! 1) :: Int
                  let x_to = read (b !! 2) :: Int
                  let y_to = read (b !! 3) :: Int
                  return (Just [(x_from,y_from),(x_to,y_to)])
                  
human    :: Chooser
human b Normal        c = do
                           move <- parse_input
                           return move
human b PawnPlacement c = return (Just [(2,2)])


 