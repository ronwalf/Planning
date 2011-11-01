{-# LANGUAGE OverlappingInstances #-}
module Main where

import System.Environment
import System.IO
import Text.ParserCombinators.Parsec (runParser)

import Planning.PDDL.PDDL3_0

main :: IO ()
main = do
    args <- getArgs
    hPutStrLn stderr $ "Parsing " ++ show args
    mapM_ (\f -> do
        ftxt <- readFile f
        let res = runParser pddlDomainParser emptyDomain f ftxt
        printResults f res) args
    where
        printResults f (Left err) = do
            putStrLn $ "Error in file " ++ f
            print err
        printResults f (Right _) = 
            putStrLn $ f ++ " OK"
