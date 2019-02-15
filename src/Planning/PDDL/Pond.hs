{-# OPTIONS_GHC
    -freduction-depth=30
  #-}
{-# LANGUAGE
    FlexibleContexts,
    TypeOperators
  #-}

module Planning.PDDL.Pond (
    module Planning.PDDL.Representation,
    module Planning.PDDL.PDDL3_0,
    PondProblem, pondProblemParser,
    POInitLiteral, POInitLiteralExpr, poInitLiteralParser
) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T

import Planning.PDDL.Representation
import Planning.PDDL.Parser
import Planning.PDDL.PDDL3_0
    (ConstTermExpr, constTermParser,
    ConstraintGDExpr, constraintGDParser,
    InitLiteral,
    PreferenceGDExpr, prefGDParser)

type POInitLiteral =
    OneOf :+:
    Unknown :+:
    InitLiteral

type POInitLiteralExpr = Expr POInitLiteral
poInitLiteralParser :: (OneOf :<: f,
        Unknown :<: f,
        Not :<: f,
        Atomic ConstTermExpr :<: f) =>
    T.TokenParser st
    -> CharParser st (Expr f)
poInitLiteralParser myLex =
    let thisP = poInitLiteralParser myLex in
    oneOfParser myLex thisP <|>
    unknownParser myLex thisP <|>
    notParser myLex thisP <|>
    atomicParser myLex (constTermParser myLex)

type PondProblem = Problem POInitLiteralExpr PreferenceGDExpr ConstraintGDExpr

pondProblemParser :: CharParser PondProblem PondProblem
pondProblemParser =
    let
        constP = constParser pddlExprLexer :: CharParser PondProblem ConstTermExpr
        initP = stdStateParser pddlExprLexer constP :: CharParser PondProblem POInitLiteralExpr
        goalP = prefGDParser pddlExprLexer :: CharParser PondProblem PreferenceGDExpr
        constraintP = constraintGDParser pddlExprLexer :: CharParser PondProblem ConstraintGDExpr
    in
    problemParser pddlDescLexer $
    problemInfoParser pddlDescLexer initP goalP constraintP
