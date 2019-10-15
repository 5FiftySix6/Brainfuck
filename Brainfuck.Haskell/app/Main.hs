{-# LANGUAGE OverloadedStrings #-}

module Main where


import Control.Exception
import Control.Monad
import Lib
import System.Environment

newtype UnmatchedArguments = UnmatchedArguments String
    deriving Show

instance Exception UnmatchedArguments

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName

    unless (length args == 1) $ throw $ UnmatchedArguments ("Invalid arguments! Usage: " <> progName <> " FILE")

    content <- readFile (head args) 

    parseRun content

    return ()