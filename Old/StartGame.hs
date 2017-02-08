-- This module interprets the user's input and plugs it into the 
-- game state after some error checking and some processing

module StartGame (startGame, 
                  getPlayerInput) where 


import ApocTools
import Text.Read

{- GUI MESSAGES -}
-- regular messages
startGameMsg = "\nLet the Apocalyse begin! The board follows a coordinate system where \n columns are from 0 though 4 and rows are from 0 through 4."

-- error messages
coordinateIndexErrMsg = "Index of your coordinates is out of range. Only 0 through 4 are allowed."


-- start the game by calling the game start function
-- this function is protected by the error checking 
-- from the previous functions before it.
startGame :: [Char] -> [Char] -> IO ()
startGame player1 player2 = do
                                -- Display start game message
                                putStrLn startGameMsg
                                -- Pring the initial board
                                print initBoard
                                -- Go into the game loop
                                gameLoop initBoard False
                                return ()

-- The idea is to use game loop along with parsemove to trickle down 
-- the game processing since we can't return data cause of IO() monads
-- so the solution is to keep passing the data and build on it. In the
-- end we call the gameLoop with the current game state and whether we
-- have determined whether the game should end or not. All gl_ functions 
-- are relevent to the gameloop flow. 
gameLoop :: GameState -> Bool -> IO()
gameLoop currBoard endGame = do 
                                if (endGame == False) then
                                    
                                    do 
                                        -- get the data from the command line
                                        -- incoming data is a String/[Char]
                                        p1move <- (getPlayerInput 1)
                                        p2move <- (getPlayerInput 2)
                                        
                                        -- send it to be parsed then pushed into 
                                        -- the current game state
                                        gl_parseMove p1move p2move

                                else 
                                
                                    endGameScene
                                
                                return ()

gl_parseMove :: [Char] -> [Char] -> IO()
gl_parseMove p1move p2move = do
                            
                            -- interpret each player's move
                            
                            -- build a gamestate data type here using
                            -- miscellaneous checking functions
                            
                            -- print the current play description 
                            -- print the board
                            
                            -- call currBoard with the current board state
                            -- that was punched earlier into the game state data
                            putStrLn "Finish"
                            
                            return()                        


-- Capture player 1 input. Player 2 function is the same. These functions are independent
-- and can be called from any where as long as this module is imported.                          
getPlayerInput :: Int -> IO String            
getPlayerInput player = do 
                    putStrLn ("\nPlease enter Player " ++ show (player) ++ "'s move (From X From Y To X To Y) Press Enter to Pass:")
                    -- get the user input in what ever form it comes
                    pMove <- getLine
                    -- check if user wants to pass or make a move.
                    -- if the length of the input is 0 then it is pass
                    -- if the length of the input is > 0 then it is probably a move
                    result <- case compare (length (pMove)) 0 of EQ -> return ("Pass")
                                                                 GT -> do 
                                                                            moveTest <- checkInput pMove
                                                                            if (moveTest == False) then 
                                                                                getPlayerInput player
                                                                            else 
                                                                                return pMove
                    return result

-- This function is not to check for valid move. This simply helps the user out. No penalties
-- are awarded because the user may have simply inserted a wrong format of the data by accident
-- this function checks for the following:         
checkInput :: [Char]-> IO Bool
checkInput input
        -- check to see if Integers were entered at all to begin with
        | length move <= 0 = do 
                                putStrLn ("Only 4 integers are allowed.")
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


{- UTILITY FUNCTIONS -}
-- this function simply converts a list of chars to list of int
-- returns empty if there were no char versions of the strings provided
-- How it works:
-- It pipes in all the data coming in and tries to parse it as an int.
-- It checks in the predicate wether the data coming in can be parsed into 
-- an int. If not then don't bother piping it into the read x because it will 
-- throw a massive an ugly error.
stringsToInt :: [Char] -> [Int]
stringsToInt xs = [read x :: Int | x <- words xs, ((readMaybe x :: Maybe Int) /= Nothing)]


endGameScene = putStrLn "<< Game Over >>"
