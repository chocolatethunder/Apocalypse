module Main (
      -- * Main
      main, main'
      ) where

import System.Environment
import Data.List
import System.IO.Unsafe


-- | The main entry, which just calls 'main'' with the command line arguments.
main = main' (unsafePerformIO getArgs)
      
      
main'           :: [String] -> IO()
main' args = do
       args <- getArgs
       case args of
               [] -> interactiveMode
               -- (x:xs) -> putStrLn (show $ (length args))
               (x:xs) -> if (length args == 2) then putStrLn "= 2" else putStrLn "error"
       

interactiveMode :: IO ()
interactiveMode = do 
        putStrLn "\nPossible strategies:\nhuman\ngreedy"
        putStrLn "Enter the strategy for BLACK:"
        blackStrategy <- getLine
        putStrLn blackStrategy
        putStrLn "Enter the strategy for WHITE:"
        whiteStrategy <- getLine
        putStrLn whiteStrategy
        
        
