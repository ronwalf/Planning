{-# OPTIONS_GHC
    -fcontext-stack=30
  #-}
{-# LANGUAGE
    FlexibleContexts,
    TypeOperators
  #-}
module Planning.PDDL.NDPDDL (
    module Planning.PDDL.PDDL3_0,
    NDPDDLDomain,
    NDPDDLItem,
    NDPDDLAction,
    NDEffectD,
    NDEffectDExpr,
    ndpddlDomainParser
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token as T

import Planning.PDDL.Parser
import Planning.PDDL.PDDL3_0

type NDEffectD =
    OneOf :+: EffectD

type NDEffectDExpr = Expr NDEffectD

ndEffectDParsing :: (OneOf :<: f,
        Not :<: f,
        And :<: f,
        When GDExpr :<: f,
        ForAll TypedVarExpr :<: f,
        Atomic TermExpr :<: f) =>
    TokenParser st
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
ndEffectDParsing mylex thisP =
    oneOfParser mylex thisP <|>
    effectDParsing mylex thisP

ndEffectDParser :: T.TokenParser a -> CharParser a NDEffectDExpr
ndEffectDParser mylex =
    ndEffectDParsing mylex (ndEffectDParser mylex)


type NDPDDLAction = Action PreferenceGDExpr NDEffectDExpr

type NDPDDLItem = Expr (DomainItem NDPDDLAction)

type NDPDDLDomain = Domain ConstraintGDExpr NDPDDLItem


ndpddlDomainParser :: CharParser NDPDDLDomain NDPDDLDomain
ndpddlDomainParser =
    let
        constraintP = constraintGDParser pddlLexer :: CharParser NDPDDLDomain ConstraintGDExpr
        prefGDP = prefGDParser pddlLexer :: CharParser NDPDDLDomain PreferenceGDExpr
        effectP = ndEffectDParser pddlLexer :: CharParser NDPDDLDomain NDEffectDExpr
    in
    domainParser pddlLexer
        (domainInfoParser pddlLexer constraintP)
        (actionParser pddlLexer prefGDP effectP)

