{-# LANGUAGE OverlappingInstances#-}
module Planning.PDDL.Parser where

import Control.Monad
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Text.ParserCombinators.Parsec.Token as T

import Planning.PDDL.Representation

comparisons = ["-", "/", "*", "<", ">", "=", ">=", "<="]

pddlLanguage = Token.LanguageDef {
    Token.commentStart = "",
    Token.commentEnd = "",
    Token.commentLine = ";",
    Token.nestedComments = False,
    Token.identStart = letter,
    Token.identLetter = (alphaNum <|> oneOf "_-"),
    Token.opStart = oneOf [],
    Token.opLetter = oneOf [],
    Token.reservedNames = [ "-", -- Types
        "and", "exists", "forall", "imply", "not", "when", -- Conditions
        ":constants", ":constraints", "define", "domain", ":functions", ":predicates", ":requirements", ":types", -- Domain info
        ":objects", "problem", ":domain",-- Problem info
        ":action", ":effect", ":parameters", ":precondition" -- Action info
        ] ++ comparisons,
    Token.reservedOpNames = [],
    Token.caseSensitive = False
}

lexer :: Token.TokenParser a
lexer = Token.makeTokenParser pddlLanguage

{-
whiteSpace= Token.whiteSpace lexer
lexeme    = Token.lexeme lexer
symbol    = Token.symbol lexer
natural   = Token.natural lexer
parens    = Token.parens lexer
semi      = Token.semi lexer
identifier= Token.identifier lexer
reserved  = Token.reserved lexer
variable  = char '?' >> identifier
-}

{-
parseTyped :: ((:<:) f g, (:<:) (Typed (Expr f)) g) => Token.TokenParser a -> GenParser Char a (Expr f) -> GenParser Char a (Expr g)
parseTyped mylex itemP = do
    item <- itemP
    itemType <- option Nothing (do
        T.reserved mylex "-"
        liftM Just $ T.identifier mylex
        )
    let t = case itemType of
            Just tname -> typed item tname
            Nothing -> item
    return t

parseTypedList :: ((:<:) f g, (:<:) (Typed (Expr f)) g) => Token.TokenParser a -> GenParser Char a (Expr f) -> GenParser Char a ([Expr g])
parseTypedList mylex el = many (parseTyped mylex el)
-}

parseTypedConst :: Token.TokenParser a -> GenParser Char a TypedConstExpr
parseTypedConst mylex = do
    In (Const cstr) <- constParser mylex
    option (eConst cstr) (do
        T.reserved mylex "-"
        tstr <- T.identifier mylex
        return $ eTyped (eConst cstr :: Expr Const) (eConst tstr :: Expr Const))


parseTypedVar :: Token.TokenParser a -> GenParser Char a TypedVarExpr
parseTypedVar mylex = do
    In (Var vstr) <- varParser mylex
    option (eVar vstr) (do
        T.reserved mylex "-"
        tstr <- T.identifier mylex
        return $ eTyped (eVar vstr :: Expr Var) (eConst tstr :: Expr Const))

-- | The domain parser takes a lexer, an domain item parser, and a domain sink
domainParser mylex domainInfoParser domainItemParser = T.whiteSpace mylex >> T.parens mylex (do
    T.reserved mylex "define"
    T.parens mylex $ (do
        T.reserved mylex "domain"
        name <- T.identifier mylex
        updateState (setName name)
        )
    many $ T.parens mylex (
        domainInfoParser
        <|>
        domainItemParser
        )
    updateState (\d -> setItems (reverse $ getItems d) d)
    getState
    )

problemParser mylex probInfoParser = T.whiteSpace mylex >> T.parens mylex (do
    T.reserved mylex "define"
    T.parens mylex $ (do
        T.reserved mylex "problem"
        name <- T.identifier mylex
        updateState (setName name)
        )
    many $ T.parens mylex probInfoParser
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

--termParser :: Token.TokenParser a -> CharParser a TermExpr
--termParser mylex = 
--    (varParser mylex) <|> (constParser mylex) <|> (functionParser mylex $ termParser mylex)

predicateParser mylex =
    (choice [T.reserved mylex c >> return c | c <- comparisons])
    <|>
    T.identifier mylex

stdStateParser mylex termP = 
    T.parens mylex $ 
    atomicParser mylex termP


conditionParser :: forall a p f. (
    (:<:) And f,
    (:<:) Or f,
    (:<:) Not f,
    (:<:) (Exists TypedVarExpr) f,
    (:<:) (ForAll TypedVarExpr) f,
    (:<:) (Atomic (Expr p)) f,
    (:<:) Var p,
    (:<:) Const p,
    (:<:) Function p
    ) => Token.TokenParser a -> CharParser a (Expr p) -> CharParser a (Expr f)
conditionParser mylex termP = 
    --(atomicFormulaParser mylex (termParser mylex :: CharParser a (Expr p)) :: CharParser a (Expr f))
    atomicParser mylex termP
{-
    (do
        try $ T.reserved mylex "and"
        parts <- many $ T.parens mylex $ (conditionParser mylex :: CharParser a (Expr f))
        return $ eAnd parts)
    <|>
    (do
        try $ T.reserved mylex "or"
        parts <- many $ T.parens mylex $ (conditionParser mylex :: CharParser a (Expr f))
        return $ eOr parts)
    <|>
    (do
        try $ T.reserved mylex "not"
        part <- T.parens mylex $ (conditionParser mylex :: CharParser a (Expr f))
        return $ eNot part)
    <|>
    (do
        try $ T.reserved mylex "forall"
        vars <- T.parens mylex $ many $ parseTypedVar mylex
        cond <- conditionParser mylex :: CharParser a (Expr f)
        return $ (eForAll vars cond :: Expr f))
    <|>
    (do
        try $ T.reserved mylex "exists"
        vars <- T.parens mylex $ many $ parseTypedVar mylex
        cond <- (conditionParser mylex :: CharParser a (Expr f))
        return $ eExists vars cond)
    <|>
    (atomicFormulaParser mylex (termParser mylex :: CharParser a (Expr p)) :: CharParser a (Expr f))
-}

atomicParser mylex termP = do
    name <- predicateParser mylex
    arguments <- many $ termP
    return $ eAtomic name arguments

andParser mylex exprP = do
    try $ T.reserved mylex "and"
    parts <- many $ T.parens mylex exprP
    return $ eAnd parts

orParser mylex exprP = do
    try $ T.reserved mylex "or"
    parts <- many $ T.parens mylex exprP
    return $ eOr parts

notParser mylex exprP = do
    try $ T.reserved mylex "not"
    part <- T.parens mylex exprP
    return $ eNot part

implyParser mylex exprP = do
    try $ T.reserved mylex "imply"
    cond <- T.parens mylex exprP
    res <- T.parens mylex exprP
    return $ eImply cond res

whenParser mylex condP exprP = do
    try $ T.reserved mylex "when"
    cond <- T.parens mylex condP
    eff <- T.parens mylex exprP
    return $ eWhen cond eff

forallParser mylex exprP = do
    try $ T.reserved mylex "forall"
    vars <- T.parens mylex $ many $ parseTypedVar mylex
    cond <- T.parens mylex exprP
    return $ eForAll vars cond

existsParser mylex exprP = do
    try $ T.reserved mylex "exists"
    vars <- T.parens mylex $ many $ parseTypedVar mylex
    cond <- T.parens mylex exprP
    return $ eExists vars cond

preferenceParser mylex exprP = do
    try $ T.reserved mylex "preference"
    name <- optionMaybe (T.identifier mylex)
    exp <- T.parens mylex exprP
    return $ ePreference name exp

oneOfParser mylex exprP = do
    try $ T.reserved mylex "oneOf"
    parts <- many1 $ T.parens mylex exprP
    return $ eOneOf parts

unknownParser mylex exprP = do
    try $ T.reserved mylex "unknown"
    part <- T.parens mylex exprP
    return $ eUnknown part


maybeParser mylex p =
    (do
        try $ T.parens mylex $ T.whiteSpace mylex
        return Nothing)
    <|>
    (do
        result <- T.parens mylex p
        return $ Just result)


--domainInfoParser :: 
--    Token.TokenParser StandardDomain -> CharParser StandardDomain (Expr f) -> CharParser StandardDomain ()
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





collect collector parser =
    (parser collector >>= (\x -> collect x parser ))
    <|>
    return collector

actionParser mylex precondP effectP = do
    let infoParser = actionInfoParser mylex precondP effectP
    try $ T.reserved mylex ":action"
    name <- T.identifier mylex
    updates <- many infoParser
    let action = foldl (\ a t -> t a) (setName name defaultAction) updates
    updateState (\d -> setItems (domainItem action : getItems d) d)

--    Token.TokenParser a -> CharParser a f -> CharParser a (Record r -> Record r)
actionInfoParser mylex precondP effectP =
    paramParser mylex
    <|>
    precondParser mylex precondP
    <|>
    effectParser mylex effectP

effectParser mylex condParser = do
    try $ T.reserved mylex ":effect"
    effect <- maybeParser mylex condParser
    return $ setEffect effect

paramParser mylex = do
    try $ T.reserved mylex ":parameters"
    params <- T.parens mylex $ many $ parseTypedVar mylex
    return $ setParameters params

precondParser mylex condParser = do
    try $ T.reserved mylex ":precondition"
    precond <- maybeParser mylex condParser
    return $ setPrecondition precond

