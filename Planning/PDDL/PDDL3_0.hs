{-# LANGUAGE
    FlexibleContexts,
    TypeOperators
  #-}
module Planning.PDDL.PDDL3_0 (
    module Planning.PDDL.Representation,
    Term, TermExpr, termParser,
    ConstTerm, ConstTermExpr, constTermParser,
    InitLiteral, InitLiteralExpr, initLiteralParser,

    PDDLAtom, pddlAtomParser,

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

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T

import Planning.PDDL.Parser
import Planning.PDDL.Representation

type Term = Const :+: Var :+: Function
type TermExpr = Expr Term
--deriving instance Data TermExpr
termParser :: T.TokenParser a -> CharParser a TermExpr
termParser myLex =
    let thisP = termParser myLex in
    constParser myLex <|>
    varParser myLex <|>
    functionParser myLex thisP

type ConstTerm = Const :+: Function
type ConstTermExpr = Expr ConstTerm
--deriving instance Data ConstTermExpr
constTermParser :: T.TokenParser a -> CharParser a ConstTermExpr
constTermParser myLex =
    constParser myLex <|>
    functionParser myLex (constTermParser myLex)

type PDDLAtom = Atomic TermExpr
pddlAtomParser :: (Atomic TermExpr :<: f) =>
    T.TokenParser st -> CharParser st (Expr f)
pddlAtomParser myLex = atomicParser myLex $ termParser myLex

type InitLiteral =
    Atomic ConstTermExpr :+:
    Not :+:
    At (Expr Const) -- By bnf, this must be a number.
type InitLiteralExpr = Expr InitLiteral
initLiteralParser :: T.TokenParser a -> CharParser a InitLiteralExpr
initLiteralParser myLex =
    let thisP = initLiteralParser myLex in
    notParser myLex thisP <|>
    atomicParser myLex (constTermParser myLex)


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

gdParsing :: (Not :<: f,
        And :<: f,
        Or :<: f,
        Imply :<: f,
        Exists TypedVarExpr :<: f,
        ForAll TypedVarExpr :<: f,
        Atomic TermExpr :<: f) =>
    T.TokenParser st
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
gdParsing myLex thisP =
    notParser myLex thisP <|>
    andParser myLex thisP <|>
    orParser myLex thisP <|>
    implyParser myLex thisP <|>
    existsParser myLex thisP <|>
    forallParser myLex thisP <|>
    pddlAtomParser myLex
gdParser :: T.TokenParser a -> CharParser a GDExpr
gdParser myLex =
    gdParsing myLex (gdParser myLex)


type PreferenceGD = 
    Preference :+:
    GD 
type PreferenceGDExpr = Expr PreferenceGD
--deriving instance Data PreferenceGDExpr

prefGDParsing :: (Preference :<: f,
        Not :<: f,
        And :<: f,
        Or :<: f,
        Imply :<: f,
        Exists TypedVarExpr :<: f,
        ForAll TypedVarExpr :<: f,
        Atomic TermExpr :<: f) =>
    T.TokenParser st
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
prefGDParsing myLex thisP =
    preferenceParser myLex thisP <|>
    gdParsing myLex thisP
prefGDParser :: T.TokenParser a -> CharParser a PreferenceGDExpr
prefGDParser myLex =
    prefGDParsing myLex (prefGDParser myLex)

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

-- TODO: Not done yet!
constraintGDParsing :: (At (Expr TimeSpecifier) :<: f,
        Always :<: f,
        Sometime :<: f,
        Within :<: f, 
        AtMostOnce :<: f,
        SometimeAfter :<: f,
        SometimeBefore :<: f,
        AlwaysWithin :<: f,
        HoldDuring :<: f,
        HoldAfter :<: f,
        Preference :<: f,
        Not :<: f,
        And :<: f,
        Or :<: f,
        Imply :<: f,
        Exists TypedVarExpr :<: f,
        ForAll TypedVarExpr :<: f,
        Atomic TermExpr :<: f) =>
    T.TokenParser st
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
constraintGDParsing myLex thisP =
    prefGDParsing myLex thisP
constraintGDParser :: T.TokenParser a -> CharParser a ConstraintGDExpr
constraintGDParser myLex =
    constraintGDParsing myLex (constraintGDParser myLex)

type EffectD =
    PDDLAtom :+:
    Not :+:
    And :+:
    When GDExpr :+:
    ForAll TypedVarExpr
type EffectDExpr = Expr EffectD
--deriving instance Data EffectDExpr

effectDParsing :: (Not :<: f,
        And :<: f,
        When GDExpr :<: f,
        ForAll TypedVarExpr :<: f,
        Atomic TermExpr :<: f) =>
    T.TokenParser st
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
effectDParsing myLex thisP =
    notParser myLex thisP <|>
    andParser myLex thisP <|>
    whenParser myLex (gdParser myLex) thisP <|>
    forallParser myLex thisP <|>
    pddlAtomParser myLex
effectDParser :: T.TokenParser a -> CharParser a EffectDExpr
effectDParser myLex =
    effectDParsing myLex (effectDParser myLex)



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
        constraintP = constraintGDParser pddlLexer :: CharParser PDDLDomain ConstraintGDExpr
        prefGDP = prefGDParser pddlLexer:: CharParser PDDLDomain PreferenceGDExpr
        effectP = effectDParser pddlLexer :: CharParser PDDLDomain EffectDExpr
    in
    domainParser pddlLexer
        (domainInfoParser pddlLexer constraintP)
        (actionParser pddlLexer prefGDP effectP)


pddlProblemParser :: CharParser PDDLProblem PDDLProblem
pddlProblemParser =
    let
        -- constP = constParser pddlLexer :: CharParser PDDLProblem ConstTermExpr
        initP = T.parens pddlLexer $ initLiteralParser pddlLexer :: CharParser PDDLProblem InitLiteralExpr
        goalP = prefGDParser pddlLexer :: CharParser PDDLProblem PreferenceGDExpr
        constraintP = constraintGDParser pddlLexer :: CharParser PDDLProblem ConstraintGDExpr
    in
    problemParser pddlLexer $
    problemInfoParser pddlLexer initP goalP constraintP

