-- This module takes the strategies from the player and punches them into
-- the game and then moves on to start the game.
module GameEngine (checkStartMode) where 


import StartGame
                   
{- SHARED DATA -}
-- Allowes strategies/modes                   
strats = ["Human", "Computer", "Random"]

{- GUI MESSAGES -}
-- regular messages
welcomeMsg = "\nWelcome to the Apocalypse! You may select one (1) of \nthe following strategies for each of the two players:"
blackStrat = "What strategy would you like to choose for the BLACK player"
whiteStrat = "What strategy would you like to choose for the WHITE player"

-- error messages
wrongStratNumMsg = "\nYou can only enter exactly two (2) strategies. \nOnly the following strategies are allowed:"
stratErrorMsg = "\nYou have entered one or more incorrect strategies. \nOnly the following strategies are allowed:"
gameOverMsg = "<< GAME OVER >>"

 
{- FUNCTIONS -}
-- This function makes sure that there are only 0 or 2 inputs that 
-- are allowed to go through. Error checking built in.
checkStartMode :: [[Char]] -> IO ()
checkStartMode strats 
        | lengthOfArgs == 0 = interactiveMode
        | lengthOfArgs == 2 = checkStrat (head strats) (last strats)
        | otherwise         = stratNumInputError
        where lengthOfArgs = length strats

-- Process two argument style only
checkStrat :: [Char] -> [Char] -> IO ()
checkStrat player1Strat player2Strat = 
                                if ((player1Strat `elem` strats) && (player2Strat `elem` strats)) 
                                    -- start game with the two strategies
                                    then startGame player1Strat player2Strat
                                    -- fail and quit
                                    else stratInputError

-- handle strategy input error
stratNumInputError :: IO ()
stratNumInputError = do 
                    putStrLn wrongStratNumMsg
                    printStrats strats
                    putStrLn gameOverMsg
                    
stratInputError :: IO ()
stratInputError = do 
                    putStrLn stratErrorMsg
                    printStrats strats
                    putStrLn gameOverMsg

-- engage Interactive Mode to capture the 2 player strategies
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
                    
                    
                    
                 
{- UTILITY FUNCTIONS -}                    
-- this prints the list of strats. This is dynamic so if we change the
-- list of strats or give them a new name above. It will automatically 
-- update down here as well. 
printStrats :: [[Char]] -> IO ()
printStrats [] = putStrLn ""
printStrats (x:xs) = do 
                    putStrLn (" " ++ x) -- meets the "print 2 spaces before" requirement
                    printStrats xs

