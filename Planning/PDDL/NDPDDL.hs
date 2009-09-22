module Planning.PDDL.NDPDDL (
    module Planning.PDDL.PDDL3_0,
    NDEffectD,
    ndpddlDomainParser
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token as T

import Planning.PDDL.Parser
import Planning.PDDL.PDDL3_0

type NDEffectD =
    OneOf :+: EffectD

type NDEffectDExpr = Expr NDEffectD

ndEffectDParsing mylex thisP =
    oneOfParser mylex thisP <|>
    effectDParsing mylex thisP

ndEffectDParser :: T.TokenParser a -> CharParser a NDEffectDExpr
ndEffectDParser mylex =
    ndEffectDParsing mylex (ndEffectDParser mylex)


type NDPDDLAction = Action PreferenceGDExpr NDEffectDExpr

type NDPDDLItems = Expr (DomainItem NDPDDLAction)

type NDPDDLDomain = Domain ConstraintGDExpr NDPDDLItems


ndpddlDomainParser :: CharParser NDPDDLDomain NDPDDLDomain
ndpddlDomainParser =
    let
        constraintP = constraintGDParser lexer :: CharParser NDPDDLDomain ConstraintGDExpr
        prefGDP = prefGDParser lexer :: CharParser NDPDDLDomain PreferenceGDExpr
        effectP = ndEffectDParser lexer :: CharParser NDPDDLDomain NDEffectDExpr
    in
    domainParser lexer
        (domainInfoParser lexer constraintP)
        (actionParser lexer prefGDP effectP)

