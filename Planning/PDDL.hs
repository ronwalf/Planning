module Planning.PDDL(
    module Planning.PDDL.Representation,
    parsePDDL
) where

import Text.ParserCombinators.Parsec

import Planning.PDDL.Representation
import Planning.PDDL.Parser

instance DomainInfoSink Domain Condition DomainItem where
    setDomainName n (Domain (d, s)) = Domain (d { domainName = n }, s)
    setRequirements r (Domain (d, s)) = Domain (d { requirements = r }, s)
    setTypes t (Domain (d, s)) = Domain (d { types = t }, s)
    setConstants c (Domain (d, s)) = Domain (d { constants = c }, s)
    setPredicates p (Domain (d, s)) = Domain (d { predicates = p }, s)
    setFunctions f = id
    setConstraints c = id
    addAction a (Domain (d, s)) = Domain (d, a:s)

instance ActionInfoSink DomainItem Condition where
    newAction name = emptyAction { actionName = name }
    setParameters params action = action { parameters = params }
    setPrecondition cond action = action { precondition = cond }
    setEffect effect action = action { effect = effect }

instance ConditionFactory Condition Term where
    makeAtomic = Atomic
    makeTrue = Empty
    makeNegation = Not
    makeConjunct = And
    makeDisjunct = Or
    makeUniversal = ForAll
    makeExistential = Exists
    makeImplies = Imply
    makeWhen = When

instance TermFactory Term where
    makeVar = Var
    makeConst = Const

parsePDDL :: String -> String -> Either ParseError Domain
parsePDDL source input =
    runParser standardParser emptyDomain source input
  
reprint source input =
    printResult $ parsePDDL source input
    where
        printResult (Left err) = print err
        printResult (Right domain) = print domain
