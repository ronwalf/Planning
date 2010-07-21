{-# OPTIONS_GHC
    -Wall
  #-}
{-# LANGUAGE
    FlexibleContexts
  #-}
module Planning.PDDL.FF ( ffOutParser ) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T

import Planning.PDDL.Parser
import Planning.PDDL.PDDL3_0

skipTill :: GenParser Char st a -> GenParser Char st ()
skipTill end =
    (end >> return ())
    <|>
    (anyChar >> skipTill end)

ffOutParser :: (Atomic (Expr Const) :<: f) 
    => CharParser () (Maybe [Expr f])
ffOutParser = do
    skipTill (newline >> string "ff:")
    msg <- manyTill anyChar newline
    case msg of
        "found legal plan as follows" -> planParser >>= return . Just
        "goal can be simplified to TRUE. The empty plan solves it" -> return $ Just []
        _ -> ffOutParser

planParser :: (Atomic (Expr Const) :<: f)
    => CharParser () [Expr f]
planParser = do
    spaces
    _ <- string "step"
    spaces
    many stepParser

stepParser :: (Atomic (Expr Const) :<: f)
    => CharParser () (Expr f)
stepParser = do
   skipMany1 digit
   _ <- char ':'
   _ <- space
   act <- T.identifier pddlLexer
   args <- manyTill (space >> constParser pddlLexer :: CharParser () (Expr Const)) newline
   return $ eAtomic act args

