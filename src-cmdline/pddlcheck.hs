{-# OPTIONS_GHC
  -freduction-depth=40
  #-}
module Main where

import Control.Monad (when)
import Data.Text.Prettyprint.Doc
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
--import Text.ParserCombinators.Parsec.Char (noneOf)
--(runParser, parse, (<|>), ParseError, CharParser)
import qualified Text.ParserCombinators.Parsec.Token as T

import Planning.PDDL.PDDL3_0

reparse :: (PDDLDoc a, Eq a, Monad m) =>
    (String -> String -> m a) -> String -> String -> m (a,a)
reparse aparser fname ftxt = do
    parsed <- aparser fname ftxt
    reparsed <- aparser "parsed" (show $ pddlDoc parsed)
    return (parsed, reparsed)

reparseCheck :: (PDDLDoc a, Eq a, Monad m) =>
    (String -> String -> m a) -> String -> String -> m (Doc ann)
reparseCheck aparser fname ftxt = do
    (parsed, reparsed) <- reparse aparser fname ftxt
    when (parsed /= reparsed) $
        fail $ "Parsed file not equal to reparsed file:\n" ++ (show $ pddlDoc parsed) ++ "\n" ++ (show $ pddlDoc reparsed)
    return $ pddlDoc parsed



eitherParser:: String -> String -> (Either ParseError (Doc ann))
eitherParser fname ftxt =
    let
      runM :: Either ParseError (Either ParseError (Doc ann))
      runM = parse (defineP (isDomP <|> isProbP)) fname ftxt
    in
    case runM of
        Left err -> Left err
        Right (Left err) -> Left err
        Right (Right doc) -> Right doc
    where

        defineP p = do
            T.whiteSpace pddlDescLexer
            _ <- T.symbol pddlDescLexer "("
            T.reserved pddlDescLexer "define"
            doc <- T.parens pddlDescLexer p
            return doc
        isDomP :: CharParser () (Either ParseError (Doc ann))
        isDomP = do
            try $ T.reserved pddlDescLexer "domain"
            _ <- T.identifier pddlDescLexer
            return domainP
        isProbP = do
            try $ T.reserved pddlDescLexer "problem"
            _ <- T.identifier pddlDescLexer
            return problemP
        domainP :: Either ParseError (Doc ann)
        domainP = reparseCheck (runParser pddlDomainParser emptyDomain) fname ftxt
        problemP:: Either ParseError (Doc ann)
        problemP = reparseCheck (runParser pddlProblemParser emptyProblem) fname ftxt


main :: IO ()
main = do
    args <- getArgs
    hPutStrLn stderr $ "Parsing " ++ show args
    mapM_ (\f -> do
        ftxt <- readFile f
        let res = eitherParser f ftxt
        printResults res) args
    where
        printResults (Left err) = do
            fail $ show err
        printResults (Right doc) =
            print doc
