module Main where

import Control.Monad.State
import Control.Monad.Trans.Except

import Forth
import Primitives

run :: VirtualMachine -> IO ()
run vm = do
    (result, newState) <- runStateT (runExceptT execute) vm
    case result of
        Left err  -> putStrLn $ "Error: " ++ err
        _         -> return () 
    print newState
    run newState


main :: IO ()
main = run emptyVM { dictionary = primitives }
