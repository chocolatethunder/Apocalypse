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

-}

module ApocStrategyHuman (
   humanPlayer
   ) where

import ApocTools
import Language
import Functions

{- |
Retrieves input from the user in the creation of a move for RunGame
-}
humanPlayer :: Chooser
humanPlayer currBoard Normal playerType = do
                                            -- Retrieves player input for a Normal move
                                            moveData <- getPlayerInput Normal playerType
                                            -- if the data returned "Pass" then return nothing
                                            -- if the data returned a list of 1 element. Something went wrong. re-try
                                            case moveData of "Pass" -> return (Nothing)
                                                             [] -> return (Nothing)
                                                             [x] -> humanPlayer currBoard Normal playerType
                                                             -- if the data return coordinates which should be properly formatted
                                                             -- then check if it returned an empty list for nothing
                                                             -- if there is a list of tuples then return that instead
                                                             -- having with it wrapped in a Just/Maybe format first
                                                             x -> do
                                                                    let coordsList = listToTuplePair(stringsToInt moveData)
                                                                    case coordsList of [] -> return (Nothing)
                                                                                        -- wrap it in a Just so it matches the type
                                                                                       xs -> return (Just (listToTuplePair(stringsToInt moveData)))

-- Retrieves player input for a PawnPlacement
humanPlayer currBoard PawnPlacement playerType = do
                                                moveData <- getPlayerInput PawnPlacement playerType
                                                return(Just (listToTuplePair(stringsToInt moveData)))
