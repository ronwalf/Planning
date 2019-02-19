{-# OPTIONS_GHC
    -Wall
  #-}
{-# LANGUAGE
    FlexibleContexts,
    OverloadedStrings,
    TypeOperators
  #-}
module Planning.PDDL.FF ( ffOutParser ) where

import qualified Data.Text as T
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
    skipTill (try $ newline >> string "ff: ")
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
    manyTill stepParser (try $ spaces >> string "time spent")

stepParser :: (Atomic (Expr Const) :<: f)
    => CharParser () (Expr f)
stepParser = do
    spaces
    skipMany1 digit
    _ <- char ':'
    _ <- space
    act <- T.identifier pddlExprLexer
    args <- manyTill cParser (try newline)
    return $ eAtomic (T.pack act) args
    where
        cParser = do
            spaces
            name <- many1 (alphaNum <|> oneOf "_-")
            return (eConst (T.pack name) :: Expr Const)
