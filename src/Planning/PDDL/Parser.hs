{-# LANGUAGE
    FlexibleContexts,
    OverlappingInstances,
    RankNTypes
  #-}
module Planning.PDDL.Parser (
    pddlDescLanguage,
    pddlExprLanguage,
    pddlDescLexer,
    pddlExprLexer,
    parseTypedConst,
    parseTypedVar,
    domainParser,
    domainInfoParser,
    problemParser,
    problemInfoParser,
    constParser,
    varParser,
    functionParser,
    predicateParser,
    stdStateParser,
    atomicParser,
    andParser,
    orParser,
    notParser,
    implyParser,
    whenParser,
    forallParser,
    existsParser,
    preferenceParser,
    oneOfParser,
    unknownParser,
    maybeParser,
    actionParser,
    actionInfoParser,
    effectParser,
    paramParser,
    precondParser,
    collect
) where

import Data.Data
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T

import Planning.PDDL.Representation

comparisons :: [String]
comparisons = ["-", "/", "*", "<", ">", "=", ">=", "<="]

baseLanguage :: T.LanguageDef st
baseLanguage = T.LanguageDef {
    T.commentStart = "",
    T.commentEnd = "",
    T.commentLine = ";",
    T.nestedComments = False,
    T.identStart = letter,
    T.identLetter = (alphaNum <|> oneOf "_-"),
    T.opStart = oneOf [],
    T.opLetter = oneOf [],
    T.reservedNames = [],
    T.reservedOpNames = [],
    T.caseSensitive = False
}

pddlDescLanguage :: T.LanguageDef st
pddlDescLanguage = baseLanguage {
    T.reservedNames = [ 
        "-", -- Types
        ":constants", ":constraints", "define", "domain", ":functions", ":predicates", ":requirements", ":types", -- Domain info
        ":objects", "problem", ":domain",-- Problem info
        ":action", ":effect", ":parameters", ":precondition" -- Action info
        ]
}

pddlExprLanguage :: T.LanguageDef st
pddlExprLanguage = baseLanguage {
    T.reservedNames = [
        "-", -- Types
        "and", "exists", "forall", "imply", "not", "when" -- Conditions
        ]
        ++ comparisons
}
pddlDescLexer :: T.TokenParser a
pddlDescLexer = T.makeTokenParser pddlDescLanguage

pddlExprLexer :: T.TokenParser a
pddlExprLexer = T.makeTokenParser pddlExprLanguage

parseTypedConst :: T.TokenParser a -> CharParser a TypedConstExpr
parseTypedConst mylex = do
    In (Const cstr) <- constParser mylex
    option (eConst cstr) (do
        T.reserved mylex "-"
        tstr <- T.identifier mylex
        return $ eTyped (eConst cstr :: Expr Const) (eConst tstr :: Expr Const))


parseTypedVar :: T.TokenParser a -> CharParser a TypedVarExpr
parseTypedVar mylex = do
    In (Var vstr) <- varParser mylex
    option (eVar vstr) (do
        T.reserved mylex "-"
        tstr <- T.identifier mylex
        return $ eTyped (eVar vstr :: Expr Var) (eConst tstr :: Expr Const))

-- | The domain parser takes a pddlLexer, an domain item parser, and a domain sink
domainParser :: (HasName b, HasActions t b) =>
    T.TokenParser b 
    -> CharParser b ()
    -> CharParser b ()
    -> CharParser b b
domainParser mylex infoParser domainItemParser = T.whiteSpace mylex >> T.parens mylex (do
    T.reserved mylex "define"
    T.parens mylex $ (do
        T.reserved mylex "domain"
        name <- T.identifier mylex
        updateState (setName name)
        )
    _ <- many $ T.parens mylex (
        infoParser
        <|>
        domainItemParser
        )
    updateState (\d -> setActions (reverse $ getActions d) d)
    getState
    )

problemParser :: (HasName b) => 
    T.TokenParser b 
    -> CharParser b a 
    -> CharParser b b
problemParser mylex probInfoParser = T.whiteSpace mylex >> T.parens mylex (do
    T.reserved mylex "define"
    T.parens mylex $ (do
        T.reserved mylex "problem"
        name <- T.identifier mylex
        updateState (setName name)
        )
    _ <- many $ T.parens mylex probInfoParser
    getState)

constParser :: (:<:) Const t => T.TokenParser a -> CharParser a (Expr t)
constParser mylex = T.identifier mylex >>= (\x -> return $ eConst x)

varParser :: (:<:) Var t => T.TokenParser a -> CharParser a (Expr t)
varParser mylex = char '?' >> T.identifier mylex >>= (\x -> return $ eVar x)

functionParser :: (:<:) Function t => 
    T.TokenParser a -> CharParser a (Expr t) -> CharParser a (Expr t)
functionParser mylex tp = T.parens mylex $ do
    name <- T.identifier mylex
    args <- many tp
    return $ eFunc name args

--termParser :: T.TokenParser a -> CharParser a TermExpr
--termParser mylex = 
--    (varParser mylex) <|> (constParser mylex) <|> (functionParser mylex $ termParser mylex)
predicateParser :: T.TokenParser st -> CharParser st String
predicateParser mylex =
    (choice [T.reserved mylex c >> return c | c <- comparisons])
    <|>
    T.identifier mylex

stdStateParser :: (Atomic a :<: f) =>
    T.TokenParser st
    -> CharParser st a
    -> CharParser st (Expr f)

stdStateParser mylex termP = 
    T.parens mylex $ 
    atomicParser mylex termP

atomicParser :: (Atomic a :<: f) =>
    T.TokenParser st
    -> CharParser st a
    -> CharParser st (Expr f)
atomicParser mylex termP = do
    name <- predicateParser mylex
    arguments <- many $ termP
    return $ eAtomic name arguments

andParser :: (And :<: f) =>
    T.TokenParser st
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
andParser mylex exprP = do
    try $ T.reserved mylex "and"
    parts <- many $ T.parens mylex exprP
    return $ eAnd parts

orParser :: (Or :<: f) =>
    T.TokenParser st
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
orParser mylex exprP = do
    try $ T.reserved mylex "or"
    parts <- many $ T.parens mylex exprP
    return $ eOr parts

notParser :: (Not :<: f) =>
    T.TokenParser st
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
notParser mylex exprP = do
    try $ T.reserved mylex "not"
    part <- T.parens mylex exprP
    return $ eNot part

implyParser :: (Imply :<: f) =>
    T.TokenParser st
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
implyParser mylex exprP = do
    try $ T.reserved mylex "imply"
    cond <- T.parens mylex exprP
    res <- T.parens mylex exprP
    return $ eImply cond res

whenParser :: (When a :<: f) =>
    T.TokenParser st
    -> CharParser st a
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
whenParser mylex condP exprP = do
    try $ T.reserved mylex "when"
    cond <- T.parens mylex condP
    eff <- T.parens mylex exprP
    return $ eWhen cond eff

forallParser :: (ForAll TypedVarExpr :<: f) =>
    T.TokenParser st
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
forallParser mylex exprP = do
    try $ T.reserved mylex "forall"
    vars <- T.parens mylex $ many $ parseTypedVar mylex
    cond <- T.parens mylex exprP
    return $ eForAll vars cond

existsParser :: (Exists TypedVarExpr :<: f) =>
    T.TokenParser st
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
existsParser mylex exprP = do
    try $ T.reserved mylex "exists"
    vars <- T.parens mylex $ many $ parseTypedVar mylex
    cond <- T.parens mylex exprP
    return $ eExists vars cond

preferenceParser :: (Preference :<: f) =>
    T.TokenParser st
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
preferenceParser mylex exprP = do
    try $ T.reserved mylex "preference"
    name <- optionMaybe (T.identifier mylex)
    expr <- T.parens mylex exprP
    return $ ePreference name expr

oneOfParser :: (OneOf :<: f) =>
    T.TokenParser st
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
oneOfParser mylex exprP = do
    try $ T.reserved mylex "oneOf"
    parts <- many1 $ T.parens mylex exprP
    return $ eOneOf parts

unknownParser :: (Unknown :<: f) =>
    T.TokenParser st
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
unknownParser mylex exprP = do
    try $ T.reserved mylex "unknown"
    part <- T.parens mylex exprP
    return $ eUnknown part

maybeParser :: T.TokenParser st
    -> CharParser st a
    -> CharParser st (Maybe a)
maybeParser mylex p =
    (do
        try $ T.parens mylex $ T.whiteSpace mylex
        return Nothing)
    <|>
    (do
        result <- T.parens mylex p
        return $ Just result)

domainInfoParser :: (HasRequirements st,
        HasTypes TypedConstExpr st,
        HasConstants TypedConstExpr st,
        HasConstraints a st,
        Atomic TypedVarExpr :<: f,
        HasPredicates (Expr f) st) =>
    T.TokenParser st
    -> CharParser st a
    -> CharParser st ()
domainInfoParser mylex condParser =
    (do
        try $ T.reserved mylex ":requirements"
        ids <- many (char ':' >> T.identifier mylex)
        updateState (setRequirements ids))
    <|>
    (do
        try $ T.reserved mylex ":types"
        types <- many $ parseTypedConst mylex
        updateState (setTypes types))
    <|>
    (do
        try $ T.reserved mylex ":constants"
        constants <- many $ parseTypedConst mylex 
        updateState (setConstants constants))
     <|>
    (do
        try $ T.reserved mylex ":constraints"
        conGD <- maybeParser mylex condParser
        updateState (setConstraints conGD))
    <|>
    (do
        try $ T.reserved mylex ":predicates"
        preds <- many $ T.parens mylex (atomicParser mylex (parseTypedVar mylex))
        updateState (setPredicates preds)
    )

problemInfoParser :: (HasDomainName st,
        HasRequirements st,
        HasConstants TypedConstExpr st,
        HasInitial a st,
        HasGoal a1 st,
        HasConstraints a2 st) =>
        T.TokenParser st
    -> GenParser Char st a
    -> CharParser st a1
    -> CharParser st a2
    -> CharParser st ()
problemInfoParser mylex stateParser goalParser constraintParser =
    (do
        try $ T.reserved mylex ":domain"
        name <- T.identifier mylex
        updateState (setDomainName name))
    <|>
    (do
        try $ T.reserved mylex ":requirements"
        ids <- many (char ':' >> T.identifier mylex)
        updateState (setRequirements ids))
    <|>
    (do
        try $ T.reserved mylex ":objects"
        objs <- many $ parseTypedConst mylex
        updateState (setConstants objs))
    <|>
    (do
        try $ T.reserved mylex ":init"
        model <- many stateParser
        updateState (setInitial model))
    <|>
    (do
        try $ T.reserved mylex ":goal"
        gd <- maybeParser mylex goalParser
        updateState (setGoal gd))
    <|>
    (do
        try $ T.reserved mylex ":constraints"
        cd <- maybeParser mylex constraintParser
        updateState (setConstraints cd))

collect ::
    a -> (a -> GenParser tok st a) -> GenParser tok st a
collect collector parser =
    (parser collector >>= (\x -> collect x parser ))
    <|>
    return collector

actionParser :: (Data p,
        Data e,
        HasActions (Action p e) st) =>
    T.TokenParser st
    -> CharParser st p
    -> CharParser st e
    -> GenParser Char st ()
actionParser mylex precondP effectP = do
    let infoParser = actionInfoParser mylex precondP effectP
    try $ T.reserved mylex ":action"
    name <- T.identifier mylex
    updates <- many infoParser
    let action = foldl (\ a t -> t a) (setName name defaultAction) updates
    updateState (\d -> setActions (action : getActions d) d)

actionInfoParser :: (HasParameters TypedVarExpr a,
        HasPrecondition a1 a,
        HasEffect a2 a) =>
    T.TokenParser st
    -> CharParser st a1
    -> CharParser st a2
    -> CharParser st (a -> a)
actionInfoParser mylex precondP effectP =
    paramParser mylex
    <|>
    precondParser mylex precondP
    <|>
    effectParser mylex effectP

effectParser :: (HasEffect a a1) =>
        T.TokenParser st
        -> CharParser st a
        -> CharParser st (a1 -> a1)
effectParser mylex condParser = do
    try $ T.reserved mylex ":effect"
    effect <- maybeParser mylex condParser
    return $ setEffect effect

paramParser :: (HasParameters TypedVarExpr a) =>
    T.TokenParser st -> CharParser st (a -> a)
paramParser mylex = do
    try $ T.reserved mylex ":parameters"
    params <- T.parens mylex $ many $ parseTypedVar mylex
    return $ setParameters params

precondParser :: (HasPrecondition a a1) =>
    T.TokenParser st
    -> CharParser st a
    -> CharParser st (a1 -> a1)
precondParser mylex condParser = do
    try $ T.reserved mylex ":precondition"
    precond <- maybeParser mylex condParser
    return $ setPrecondition precond

