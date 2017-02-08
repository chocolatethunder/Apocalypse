-- This module contain on the GLOBAL functions being used throughout the program
-- This exists because these functions are being called by different modules at
-- different times. 

module Lib.Functions where

import Text.Read

import ApocTools
import Lib.Language


-- this prints the list of strats. This is dynamic so if we change the
-- list of strats or give them a new name above. It will automatically 
-- update down here as well. 
printStrats :: [[Char]] -> IO ()
printStrats [] = putStrLn ""
printStrats (x:xs) = do 
                    putStrLn (" " ++ x) -- meets the "print 2 spaces before" requirement
                    printStrats xs


                    
                    
-- this function simply converts a list of chars to list of int
-- returns empty if there were no char versions of the strings provided
-- How it works:
-- It pipes in all the data coming in and tries to parse it as an int.
-- It checks in the predicate wether the data coming in can be parsed into 
-- an int. If not then don't bother piping it into the read x because it will 
-- throw a massive an ugly error.
stringsToInt :: [Char] -> [Int]
stringsToInt xs = [read x :: Int | x <- words xs, ((readMaybe x :: Maybe Int) /= Nothing)]


-- This function merely converts a list of ints into a pair of tuples
-- only works on list with even number of ints in it.
listToTuplePair :: [Int] -> [(Int,Int)]
listToTuplePair [] = []
listToTuplePair [x] = []
listToTuplePair (x:y:z) = (x,y) : listToTuplePair z

-- This function merely checks to see if 
isTupleInList :: (Int,Int) -> [(Int,Int)] -> Bool
isTupleInList (dX,dY) moves 
        | long == 1 = True 
        | otherwise = False
        where 
            long = length ([(fst(xs),snd(xs)) | xs <- moves, (fst(xs)==dX && snd(xs)==dY)])
            

-- Replaces the nth element in a row with a new element.
replace :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                     in (if null zs then (if null ys then [] else init ys) else ys)
                        ++ [elem]
                        ++ (if null zs then [] else tail zs)


-- Replaces the (x,y)th element in a list of lists with a new element.
replace2 :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)


-- Capture player input. These functions are independent
-- and can be called from any where as long as this module is imported.                          
getPlayerInput ::  PlayType -> Player -> IO String            
getPlayerInput playType player = do 
                                    putStrLn ("Please enter " ++ show (player) ++ " Player's move (From X From Y To X To Y) Press Enter to Pass:")
                                    -- get the user input in what ever form it comes
                                    pMove <- getLine
                                    -- check if user wants to pass or make a move.
                                    -- if the length of the input is 0 then it is pass
                                    -- if the length of the input is > 0 then it is probably a move
                                    result <- case compare (length (pMove)) 0 of EQ -> return ("Pass")
                                                                                 GT -> do 
                                                                                            moveTest <- checkInput pMove playType
                                                                                            if (moveTest == False) then 
                                                                                                getPlayerInput playType player 
                                                                                            else 
                                                                                                return pMove
                                    return result


                    
-- This function is not to check for valid move. This simply helps the user out. No penalties
-- are awarded because the user may have simply inserted a wrong format of the data by accident
-- this function checks for the following:         
checkInput :: [Char] -> PlayType -> IO Bool
checkInput input playType
        -- if it is a pawn placement move then it needs to accomodate for that move
        | ((length move == 2) && (playType == PawnPlacement)) = return (True)
        -- if length is more than 4
        | length move > 4 = do 
                                putStrLn tooManyCoordErrMsg
                                return (False)
        -- check to see if Integers were entered at all to begin with
        | length move <= 0 = do 
                                putStrLn genericCoordinateErrMsg
                                return (False)
        -- check to see if the user has entered only 4 numbers. No more no less.
        | (length move < 4) && (length move > 0) = do 
                                                    putStrLn ("You have only entered " ++ show (length move) ++ " integers, 4 are required.")
                                                    return (False)
        -- check to see if the corrdinates are in range (0 - 4)
        | (or (map (< 0) move)) || (or (map (> 4) move)) = do 
                                                            putStrLn coordinateIndexErrMsg 
                                                            return (False)
        | otherwise = return (True)
        where             
            move = stringsToInt input