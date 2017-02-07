module StartGame (startGame, 
                  getPlayer1Input, 
                  getPlayer2Input) where 

import ApocTools

{- GUI MESSAGES -}
-- regular messages
startGameMsg = "\nLet the Apocalyse begin! The board follows a coordinate system where \n columns are from 0 though 4 and rows are from 0 through 4."
p1InputPromptMsg = "\nPlease enter Player 1's move (From X From Y To X To Y) Press Enter to Pass:"
p2InputPromptMsg = "\nPlease enter Player 2's move (From X From Y To X To Y) Press Enter to Pass:"

-- error messages
coordinateIndexErrMsg = "Index of your coordinates is out of range. Only 0 through 4 are allowed."


-- start the game by calling the game start function
-- this function is protected by the error checking 
-- from the previous functions before it.
startGame :: [Char] -> [Char] -> IO ()
startGame player1 player2 = do
                                putStrLn startGameMsg
                                print initBoard
                                -- process the type of player1 and player 2 here
                                
                                p1move <- getPlayer1Input
                                p2move <- getPlayer2Input
                                
                                testFunction p1move p2move 
                                
                                return ()


-- Capture player 1 input. Player 2 function is the same. These functions are independent
-- and can be called from any where as long as this module is imported.                          
getPlayer1Input :: IO String            
getPlayer1Input = do 
                    putStrLn p1InputPromptMsg
                    p1Move <- getLine
                    -- if the input is empty then it is a pass
                    -- otherwise we check whether it is a valid input
                    case compare (length (p1Move)) 0 of EQ -> return ("Pass")
                                                        GT -> do 
                                                                moveTest <- checkInput p1Move
                                                                if (moveTest == True) then  
                                                                    return p1Move
                                                                else 
                                                                    getPlayer1Input
                                                                return("")
                    return("")

-- See getPlayer1Input comments
getPlayer2Input :: IO String            
getPlayer2Input = do 
                    putStrLn p2InputPromptMsg
                    p2Move <- getLine
                    case compare (length (p2Move)) 0 of EQ -> return ("Pass")
                                                        GT -> do 
                                                                moveTest <- checkInput p2Move
                                                                if (moveTest == True) then  
                                                                    return p2Move
                                                                else 
                                                                    getPlayer2Input
                                                                return("")
                    return("")

-- This function is not to check for valid move. This simply helps the user out. No penalties
-- are awarded because the user may have simply inserted a wrong format of the data by accident
-- this function checks for the following:         
checkInput :: [Char]-> IO Bool
checkInput input 
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
            -- pipe in all the data coming in from the commandline and parse it as an int
            -- TO DO Handle parse error. Prelude.read: no parse
            move = [read x :: Int | x <- words input]

-- Error handling function. To be completed.          
handler :: IOError -> IO ()  
handler e = putStrLn "Whoops, had some trouble!"           
            
            
testFunction :: [Char] -> [Char] -> IO ()
testFunction p1Move p2Move = do 
                                putStrLn "Finish"
                                putStrLn (show (p1Move))
                                return()