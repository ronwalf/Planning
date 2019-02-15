{-# OPTIONS_GHC
    -freduction-depth=30
  #-}
{-# LANGUAGE
    FlexibleContexts,
    TypeOperators
  #-}
module Planning.PDDL.NDPDDL (
    module Planning.PDDL.PDDL3_0,
    NDPDDLDomain,
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
        --And :<: f,
        --When GDExpr :<: f,
        --ForAll TypedVarExpr :<: f,
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

type NDPDDLEffect = ([TypedVarExpr], Maybe GDExpr, [NDEffectDExpr])
type NDPDDLAction = Action PDDLPrecond NDPDDLEffect


type NDPDDLDomain = Domain ConstraintGDExpr NDPDDLAction GDExpr


ndpddlDomainParser :: CharParser NDPDDLDomain NDPDDLDomain
ndpddlDomainParser =
    let
        constraintP = constraintGDParser pddlExprLexer :: CharParser NDPDDLDomain ConstraintGDExpr
        prefP = prefListParser pddlExprLexer (gdParser pddlExprLexer :: CharParser NDPDDLDomain GDExpr)
        effectP = ucEffectParser pddlExprLexer
            (gdParser pddlExprLexer :: CharParser NDPDDLDomain GDExpr)
            (ndEffectDParser pddlExprLexer :: CharParser NDPDDLDomain NDEffectDExpr)
    in
    domainParser pddlDescLexer $
        (domainInfoParser pddlDescLexer constraintP)
        <|>
        derivedParser pddlDescLexer
          (atomicTypeParser pddlExprLexer (varParser pddlExprLexer) :: CharParser st TypedPredicateExpr)
          (gdParser pddlExprLexer)
        <|>
        (actionParser pddlDescLexer prefP effectP)
