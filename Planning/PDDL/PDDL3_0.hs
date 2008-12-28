module Planning.PDDL.PDDL3_0 (
    module Planning.PDDL.Representation,
    Term, TermExpr, termParser,
    ConstTerm, ConstTermExpr, constTermParser,
    InitLiteral, InitLiteralExpr,

    PDDLAtom,

    GD, GDExpr,
    DAGD, DAGDExpr,
    PreferenceGD, PreferenceGDExpr, prefGDParsing, prefGDParser,
    ConstraintGD, ConstraintGDExpr, constraintGDParsing, constraintGDParser,

    EffectD, EffectDExpr, effectDParsing, effectDParser,
    DAEffectD, DAEffectDExpr,

    PDDLDomain,
    PDDLProblem,
    PDDLAction,
    PDDLItems,

    pddlDomainParser,
    pddlProblemParser

) where

import Data.Generics (Data)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T

import Planning.PDDL.Parser
import Planning.PDDL.Representation

type Term = Const :+: Var :+: Function
type TermExpr = Expr Term
--deriving instance Data TermExpr
termParser :: T.TokenParser a -> CharParser a TermExpr
termParser lex =
    let thisP = termParser lex in
    constParser lex <|>
    varParser lex <|>
    functionParser lex thisP

type ConstTerm = Const :+: Function
type ConstTermExpr = Expr ConstTerm
--deriving instance Data ConstTermExpr
constTermParser :: T.TokenParser a -> CharParser a ConstTermExpr
constTermParser lex =
    constParser lex <|>
    functionParser lex (constTermParser lex)

type PDDLAtom = Atomic TermExpr
pddlAtomParser lex = atomicParser lex $ termParser lex

type InitLiteral =
    Atomic ConstTermExpr :+:
    Not :+:
    At (Expr Const) -- By bnf, this must be a number.
type InitLiteralExpr = Expr InitLiteral
--deriving instance Data InitLiteralExpr
initLiteralParser lex =
    let thisP = initLiteralParser lex in
    notParser lex thisP <|>
    atomicParser lex (constTermParser lex)


type GD = 
    PDDLAtom :+:
    Not :+:
    And :+:
    Or :+:
    Imply :+:
    Exists TypedVarExpr :+:
    ForAll TypedVarExpr
type GDExpr = Expr GD
--deriving instance Data GDExpr

gdParsing lex thisP =
    notParser lex thisP <|>
    andParser lex thisP <|>
    orParser lex thisP <|>
    implyParser lex thisP <|>
    existsParser lex thisP <|>
    forallParser lex thisP <|>
    pddlAtomParser lex
gdParser :: T.TokenParser a -> CharParser a GDExpr
gdParser lex =
    gdParsing lex (gdParser lex)


type PreferenceGD = 
    Preference :+:
    GD 
type PreferenceGDExpr = Expr PreferenceGD
--deriving instance Data PreferenceGDExpr

prefGDParsing lex thisP =
    preferenceParser lex thisP <|>
    gdParsing lex thisP
prefGDParser :: T.TokenParser a -> CharParser a PreferenceGDExpr
prefGDParser lex =
    prefGDParsing lex (prefGDParser lex)

type TimeSpecifier = Start :+: End
--deriving instance Data (Expr TimeSpecifier)


type Interval = All
--deriving instance Data (Expr Interval)

type DAGD =
    At (Expr TimeSpecifier) :+:
    Over (Expr Interval) :+:
    PreferenceGD
type DAGDExpr = Expr DAGD
--deriving instance Data DAGDExpr

type ConstraintGD =
    At (Expr TimeSpecifier) :+:
    Always :+:
    Sometime :+:
    Within :+:
    AtMostOnce :+:
    SometimeAfter :+:
    SometimeBefore :+:
    AlwaysWithin :+:
    HoldDuring :+:
    HoldAfter :+:
    PreferenceGD
type ConstraintGDExpr = Expr ConstraintGD
--deriving instance Data ConstraintGDExpr

constraintGDParsing lex thisP =
    prefGDParsing lex thisP
constraintGDParser :: T.TokenParser a -> CharParser a ConstraintGDExpr
constraintGDParser lex =
    constraintGDParsing lex (constraintGDParser lex)

type EffectD =
    PDDLAtom :+:
    Not :+:
    And :+:
    When GDExpr :+:
    ForAll TypedVarExpr
type EffectDExpr = Expr EffectD
--deriving instance Data EffectDExpr

effectDParsing lex thisP =
    notParser lex thisP <|>
    andParser lex thisP <|>
    whenParser lex (gdParser lex) thisP <|>
    forallParser lex thisP <|>
    pddlAtomParser lex
effectDParser :: T.TokenParser a -> CharParser a EffectDExpr
effectDParser lex =
    effectDParsing lex (effectDParser lex)



type DAEffectD =
    At (Expr TimeSpecifier) :+:
    EffectD
type DAEffectDExpr = Expr DAEffectD
--deriving instance Data DAEffectDExpr


type PDDLAction = Action PreferenceGDExpr EffectDExpr

type PDDLItems = Expr (DomainItem PDDLAction)
--deriving instance Data PDDLItems

type PDDLDomain = Domain ConstraintGDExpr PDDLItems

type PDDLProblem = Problem InitLiteralExpr PreferenceGDExpr ConstraintGDExpr


pddlDomainParser :: CharParser PDDLDomain PDDLDomain
pddlDomainParser =
    let
        constraintP = constraintGDParser lexer :: CharParser PDDLDomain ConstraintGDExpr
        prefGDP = prefGDParser lexer :: CharParser PDDLDomain PreferenceGDExpr
        effectP = effectDParser lexer :: CharParser PDDLDomain EffectDExpr
    in
    domainParser lexer
        (domainInfoParser lexer constraintP)
        (actionParser lexer prefGDP effectP)


pddlProblemParser :: CharParser PDDLProblem PDDLProblem
pddlProblemParser =
    let
        constP = constParser lexer :: CharParser PDDLProblem ConstTermExpr
        initP = stdStateParser lexer constP :: CharParser PDDLProblem InitLiteralExpr
        goalP = prefGDParser lexer :: CharParser PDDLProblem PreferenceGDExpr
        constraintP = constraintGDParser lexer :: CharParser PDDLProblem ConstraintGDExpr
    in
    problemParser lexer $
    problemInfoParser lexer initP goalP constraintP

