{-# OPTIONS_GHC
  -fcontext-stack=40
  #-}
{-# LANGUAGE OverlappingInstances #-}
module Main where

import Control.Monad (liftM, when)
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Char (noneOf)
--(runParser, parse, (<|>), ParseError, CharParser)
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.PrettyPrint (Doc, sep, text)

import Planning.PDDL.PDDL3_0

reparse :: (PDDLDoc a, Eq a, Monad m) =>
    (String -> String -> m a) -> String -> String -> m (a,a)
reparse aparser fname ftxt = do
    parsed <- aparser fname ftxt
    reparsed <- aparser "parsed" (show $ pddlDoc parsed)
    return (parsed, reparsed)

reparseCheck :: (PDDLDoc a, Eq a, Monad m) =>
    (String -> String -> m a) -> String -> String -> m Doc
reparseCheck aparser fname ftxt = do
    (parsed, reparsed) <- reparse aparser fname ftxt
    when (parsed /= reparsed) $
        fail $ "Parsed file not equal to reparsed file:\n" ++ (show $ pddlDoc parsed) ++ "\n" ++ (show $ pddlDoc reparsed)
    return $ pddlDoc parsed
    
    

eitherParser:: String -> String -> (Either ParseError Doc)
eitherParser fname ftxt =
    let
      runM :: Either ParseError (Either ParseError Doc)
      runM = parse (defineP (isDomP <|> isProbP)) fname ftxt
    in
    case runM of 
        Left err -> Left err
        Right (Left err) -> Left err
        Right (Right doc) -> Right doc
    where

        defineP p = do
            T.whiteSpace pddlDescLexer
            T.symbol pddlDescLexer "("
            T.reserved pddlDescLexer "define"
            doc <- T.parens pddlDescLexer p <?> "PDDL type"
            return doc
        isDomP :: CharParser () (Either ParseError Doc)
        isDomP = do
            try $ T.reserved pddlDescLexer "domain"
            T.identifier pddlDescLexer <?> "Domain name"
            return domainP
        isProbP = do
            try $ T.reserved pddlDescLexer "problem"
            T.identifier pddlDescLexer <?> "Problem name"
            return problemP
        domainP :: Either ParseError Doc
        domainP = reparseCheck (runParser pddlDomainParser emptyDomain) fname ftxt
        problemP:: Either ParseError Doc
        problemP = reparseCheck (runParser pddlProblemParser emptyProblem) fname ftxt
            

main :: IO ()
main = do
    args <- getArgs
    hPutStrLn stderr $ "Parsing " ++ show args
    mapM_ (\f -> do
        ftxt <- readFile f
        let res = eitherParser f ftxt
        printResults f res) args
    where
        printResults f (Left err) = do
            fail $ show err
        printResults f (Right doc) = 
            print doc
