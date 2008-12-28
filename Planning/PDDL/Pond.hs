module Planning.PDDL.Pond (
    module Planning.PDDL.Representation,
    module Planning.PDDL.PDDL3_0,
    PondProblem, pondProblemParser,
    POInitLiteral, POInitLiteralExpr, poInitLiteralParser
) where

import Text.ParserCombinators.Parsec

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
poInitLiteralParser lex =
    let thisP = poInitLiteralParser lex in
    oneOfParser lex thisP <|>
    unknownParser lex thisP <|>
    notParser lex thisP <|>
    atomicParser lex (constTermParser lex)

type PondProblem = Problem POInitLiteralExpr PreferenceGDExpr ConstraintGDExpr

pondProblemParser :: CharParser PondProblem PondProblem
pondProblemParser =
    let
        constP = constParser lexer :: CharParser PondProblem ConstTermExpr
        initP = stdStateParser lexer constP :: CharParser PondProblem POInitLiteralExpr
        goalP = prefGDParser lexer :: CharParser PondProblem PreferenceGDExpr
        constraintP = constraintGDParser lexer :: CharParser PondProblem ConstraintGDExpr
    in
    problemParser lexer $
    problemInfoParser lexer initP goalP constraintP
