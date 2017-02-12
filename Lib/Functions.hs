-- This module contain the GLOBAL functions being used throughout the program
-- This exists because these functions are being called by different modules at
-- different times.

module Lib.Functions where

import Text.Read
import Control.Monad
import Data.Maybe

import ApocTools
import Lib.Language


-- this prints the list of strats. This is dynamic so if we change the
-- list of strats or give them a new name above. It will automatically
-- update down here as well.
printStrats :: [[Char]] -> IO ()
printStrats [] = putStrLn ""
printStrats (x:xs) = do
                    putStrLn ("  " ++ x) -- meets the "print 2 spaces before" requirement
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
                                                    putStrLn ("You have only entered " ++ show (length move) ++ " integers, 4 are required.\n")
                                                    return (False)
        -- check to see if the corrdinates are in range (0 - 4)
        | (or (map (< 0) move)) || (or (map (> 4) move)) = do
                                                            putStrLn coordinateIndexErrMsg
                                                            return (False)
        | otherwise = return (True)
        where
            move = stringsToInt input

-- converts the Maybe data type to (Tuple,Tuple) for Played data type
getTwoCoords :: Maybe [(Int,Int)] -> ((Int,Int),(Int,Int))
getTwoCoords coords = case coords of Nothing -> ((0,0),(0,0))
                                     maybe -> (((fromJust coords) !! 0),((fromJust coords) !! 1))

-- Generates all the possible moves of a knight on the board given it's current position. For Normal playtype only.
legalKnightMoves :: (Int,Int) -> [(Int,Int)]
legalKnightMoves (sX,sY) = filter possibleMoves [(sX+2,sY+1),(sX+2,sY-1),(sX-2,sY+1),(sX-2,sY-1),(sX+1,sY+2),(sX+1,sY-2),(sX-1,sY+2),(sX-1,sY-2)] where possibleMoves (sX,sY) = sX `elem` [0..4] && sY `elem` [0..4]


-- Generates all the possible moves of a pawn on the board given it's current position. For Normal playtype only.
legalPawnMoves :: (Int,Int) -> Player -> Bool -> [(Int,Int)]
-- sX,sY: starting x and y
-- currPlayer: White or Black pawn type
-- knockout: allowed to move diagonally to attack
legalPawnMoves (sX,sY) currPlayer knockout
            | (knockout == True && currPlayer == White) = filter possibleMoves [(sX,sY+1),(sX-1,sY+1),(sX+1,sY+1)]
            | (knockout == False && currPlayer == White) = filter possibleMoves [(sX,sY+1)]
            | (knockout == True && currPlayer == Black) = filter possibleMoves [(sX,sY-1),(sX-1,sY-1),(sX+1,sY-1)]
            | (knockout == False && currPlayer == Black) = filter possibleMoves [(sX,sY-1)]
            where
                possibleMoves (sX,sY) = sX `elem` [0..4] && sY `elem` [0..4]

-- Play type validation
-- this function checks to see if the piece is moving to a location
-- that has a character or not. It engages attack mode for the pawns.
-- it then returns whether the piece moving from src to dst is allowed
-- to do so. It also makes sure that the user can't move enemy team's player.
-- Otherwise Played becomes Goofed.
validateMove :: GameState -> Maybe [(Int,Int)] -> Player -> IO Bool
validateMove currBoard move currPlayer = do
                                -- error check to make sure we are no processing a Nothing
                                case move of Nothing -> return (False)
                                             maybe -> do

                                                    -- get the character at source pos and the player it belongs to
                                                    let from = cell2Char(getFromBoard (theBoard currBoard) ((fromJust move) !! 0))
                                                    let fromPieceOf = playerOf((pieceOf (char2Cell(from))))

                                                    -- get the character at dest pos and the player it belongs to
                                                    let to = cell2Char(getFromBoard (theBoard currBoard) ((fromJust move) !! 1))
                                                    let toPieceOf = playerOf((pieceOf (char2Cell(to))))

                                                    -- check to make sure that the user doesn't try to move
                                                    -- either an empty piece or the enemy piece or their own piece
                                                    if (from /= '_' && fromPieceOf == currPlayer) then

                                                        -- check if the player is in attack mode
                                                        if (to /= '_') then
                                                            validatePieceMove from True ((fromJust move) !! 0) ((fromJust move) !! 1)
                                                        else
                                                            validatePieceMove from False ((fromJust move) !! 0) ((fromJust move) !! 1)

                                                    else
                                                        return False


-- Validates each piece's movement. Supply it with a piece, a source, and a dest and it will
-- tell you whether it is making the right move or not. The attack mode is for the pawns so that
-- they are allowed to move diagonally.
validatePieceMove :: Char -> Bool -> (Int,Int) -> (Int,Int) -> IO Bool
validatePieceMove piece attack src dst
        | (char2Cell piece == WK) = return (isTupleInList (dX,dY) (legalKnightMoves (sX,sY)))
        | (char2Cell piece == BK) = return (isTupleInList (dX,dY) (legalKnightMoves (sX,sY)))
        | (char2Cell piece == WP && attack == False) = return (isTupleInList (dX,dY) (legalPawnMoves (sX,sY) White False))
        | (char2Cell piece == BP && attack == False) = return (isTupleInList (dX,dY) (legalPawnMoves (sX,sY) Black False))
        | (char2Cell piece == WP && attack == True) = return (isTupleInList (dX,dY) (legalPawnMoves (sX,sY) White True))
        | (char2Cell piece == BP && attack == True) = return (isTupleInList (dX,dY) (legalPawnMoves (sX,sY) Black True))
        | otherwise = return False
        where
            sX = fst src
            sY = snd src
            dX = fst dst
            dY = snd dst

-- Check if there are any Pawns left on board for a given player
arePawnsLeft :: Board -> Player -> Bool
arePawnsLeft currboard player = ((getPawnsLeft currboard player) > 0)


-- Get number of Pawns left on board for a given player
getPawnsLeft :: Board -> Player -> Int
getPawnsLeft currboard player
        | player == Black = sum [sum [ 1 | y <- x, (y == BP)] | x <- currboard]
        | player == White = sum [sum [ 1 | y <- x, (y == WP)] | x <- currboard]
        | otherwise = 0

-- specification for human prompt normal or PawnPlacement move
playerInputPrompt:: Player -> PlayType -> String
playerInputPrompt White Normal = "W2"
playerInputPrompt White PawnPlacement = "W1"
playerInputPrompt Black Normal = "B2"
playerInputPrompt Black PawnPlacement = "B1"
