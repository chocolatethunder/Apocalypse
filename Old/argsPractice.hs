module Main (
      -- * Main
      main, main'
      ) where

import System.Environment
import Data.List
import System.IO.Unsafe
import System.Exit


-- | The main entry, which just calls 'main'' with the command line arguments.
main = main' (unsafePerformIO getArgs)
legalStategies = ["human", "greedy"]
 
      
main'           :: [String] -> IO()
main' args = do
       args <- getArgs
       case args of
               [] -> interactiveMode
               -- (x:xs) -> putStrLn (show $ (length args))
               (x:xs) -> if (length args == 2) then checkLegalStrategy args else illegalStrategies
       
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
                                         