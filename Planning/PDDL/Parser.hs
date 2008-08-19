{-# OPTIONS 
 -fglasgow-exts
 -fallow-overlapping-instances 
 #-}
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
parseTyped lex itemP = do
    item <- itemP
    itemType <- option Nothing (do
        T.reserved lex "-"
        liftM Just $ T.identifier lex
        )
    let t = case itemType of
            Just tname -> typed item tname
            Nothing -> item
    return t

parseTypedList :: ((:<:) f g, (:<:) (Typed (Expr f)) g) => Token.TokenParser a -> GenParser Char a (Expr f) -> GenParser Char a ([Expr g])
parseTypedList lex el = many (parseTyped lex el)
-}

parseTypedConst :: Token.TokenParser a -> GenParser Char a TypedConstExpr
parseTypedConst lex = do
    In (Const cstr) <- constParser lex
    option (eConst cstr) (do
        T.reserved lex "-"
        tstr <- T.identifier lex
        return $ eTyped (eConst cstr :: Expr Const) (eConst tstr :: Expr Const))


parseTypedVar :: Token.TokenParser a -> GenParser Char a TypedVarExpr
parseTypedVar lex = do
    In (Var vstr) <- varParser lex
    option (eVar vstr) (do
        T.reserved lex "-"
        tstr <- T.identifier lex
        return $ eTyped (eVar vstr :: Expr Var) (eConst tstr :: Expr Const))

-- | The domain parser takes a lexer, an domain item parser, and a domain sink
domainParser lex domainInfoParser domainItemParser = T.whiteSpace lex >> T.parens lex (do
    T.reserved lex "define"
    T.parens lex $ (do
        T.reserved lex "domain"
        name <- T.identifier lex
        updateState (setName name)
        )
    many $ T.parens lex (
        domainInfoParser
        <|>
        domainItemParser
        )
    getState
    )

problemParser lex probInfoParser = T.whiteSpace lex >> T.parens lex (do
    T.reserved lex "define"
    T.parens lex $ (do
        T.reserved lex "problem"
        name <- T.identifier lex
        updateState (setName name)
        )
    many $ T.parens lex probInfoParser
    getState)

constParser :: (:<:) Const t => T.TokenParser a -> CharParser a (Expr t)
constParser lex = T.identifier lex >>= (\x -> return $ eConst x)

varParser :: (:<:) Var t => T.TokenParser a -> CharParser a (Expr t)
varParser lex = char '?' >> T.identifier lex >>= (\x -> return $ eVar x)

functionParser :: (:<:) Function t => 
    T.TokenParser a -> CharParser a (Expr t) -> CharParser a (Expr t)
functionParser lex tp = T.parens lex $ do
    name <- T.identifier lex
    args <- many tp
    return $ eFunc name args

--termParser :: Token.TokenParser a -> CharParser a TermExpr
--termParser lex = 
--    (varParser lex) <|> (constParser lex) <|> (functionParser lex $ termParser lex)

predicateParser lex =
    (choice [T.reserved lex c >> return c | c <- comparisons])
    <|>
    T.identifier lex

stdStateParser lex termP = 
    T.parens lex $ 
    atomicParser lex termP


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
conditionParser lex termP = 
    --(atomicFormulaParser lex (termParser lex :: CharParser a (Expr p)) :: CharParser a (Expr f))
    atomicParser lex termP
{-
    (do
        try $ T.reserved lex "and"
        parts <- many $ T.parens lex $ (conditionParser lex :: CharParser a (Expr f))
        return $ eAnd parts)
    <|>
    (do
        try $ T.reserved lex "or"
        parts <- many $ T.parens lex $ (conditionParser lex :: CharParser a (Expr f))
        return $ eOr parts)
    <|>
    (do
        try $ T.reserved lex "not"
        part <- T.parens lex $ (conditionParser lex :: CharParser a (Expr f))
        return $ eNot part)
    <|>
    (do
        try $ T.reserved lex "forall"
        vars <- T.parens lex $ many $ parseTypedVar lex
        cond <- conditionParser lex :: CharParser a (Expr f)
        return $ (eForAll vars cond :: Expr f))
    <|>
    (do
        try $ T.reserved lex "exists"
        vars <- T.parens lex $ many $ parseTypedVar lex
        cond <- (conditionParser lex :: CharParser a (Expr f))
        return $ eExists vars cond)
    <|>
    (atomicFormulaParser lex (termParser lex :: CharParser a (Expr p)) :: CharParser a (Expr f))
-}

atomicParser lex termP = do
    name <- predicateParser lex
    arguments <- many $ termP
    return $ eAtomic name arguments

andParser lex exprP = do
    try $ T.reserved lex "and"
    parts <- many $ T.parens lex exprP
    return $ eAnd parts

orParser lex exprP = do
    try $ T.reserved lex "or"
    parts <- many $ T.parens lex exprP
    return $ eOr parts

notParser lex exprP = do
    try $ T.reserved lex "not"
    part <- T.parens lex exprP
    return $ eNot part

implyParser lex exprP = do
    try $ T.reserved lex "imply"
    cond <- T.parens lex exprP
    res <- T.parens lex exprP
    return $ eImply cond res

whenParser lex condP exprP = do
    try $ T.reserved lex "when"
    cond <- T.parens lex condP
    eff <- T.parens lex exprP
    return $ eWhen cond eff

forallParser lex exprP = do
    try $ T.reserved lex "forall"
    vars <- T.parens lex $ many $ parseTypedVar lex
    cond <- T.parens lex exprP
    return $ eForAll vars cond

existsParser lex exprP = do
    try $ T.reserved lex "exists"
    vars <- T.parens lex $ many $ parseTypedVar lex
    cond <- T.parens lex exprP
    return $ eExists vars cond

preferenceParser lex exprP = do
    try $ T.reserved lex "preference"
    name <- optionMaybe (T.identifier lex)
    exp <- T.parens lex exprP
    return $ ePreference name exp

maybeParser lex p =
    (do
        try $ T.parens lex $ T.whiteSpace lex
        return Nothing)
    <|>
    (do
        result <- T.parens lex p
        return $ Just result)


--domainInfoParser :: 
--    Token.TokenParser StandardDomain -> CharParser StandardDomain (Expr f) -> CharParser StandardDomain ()
domainInfoParser lex condParser =
    (do
        try $ T.reserved lex ":requirements"
        ids <- many (char ':' >> T.identifier lex)
        updateState (setRequirements ids))
    <|>
    (do
        try $ T.reserved lex ":types"
        types <- many $ parseTypedConst lex
        updateState (setTypes types))
    <|>
    (do
        try $ T.reserved lex ":constants"
        constants <- many $ parseTypedConst lex 
        updateState (setConstants constants))
     <|>
    (do
        try $ T.reserved lex ":constraints"
        conGD <- maybeParser lex condParser
        updateState (setConstraints conGD))
    <|>
    (do
        try $ T.reserved lex ":predicates"
        preds <- many $ T.parens lex (atomicParser lex (parseTypedVar lex))
        updateState (setPredicates preds)
    )

problemInfoParser lex stateParser goalParser constraintParser =
    (do
        try $ T.reserved lex ":domain"
        name <- T.identifier lex
        updateState (setDomainName name))
    <|>
    (do
        try $ T.reserved lex ":requirements"
        ids <- many (char ':' >> T.identifier lex)
        updateState (setRequirements ids))
    <|>
    (do
        try $ T.reserved lex ":objects"
        objs <- many $ parseTypedConst lex
        updateState (setConstants objs))
    <|>
    (do
        try $ T.reserved lex ":init"
        model <- many stateParser
        updateState (setInitial model))
    <|>
    (do
        try $ T.reserved lex ":goal"
        gd <- maybeParser lex goalParser
        updateState (setGoal gd))
    <|>
    (do
        try $ T.reserved lex ":constraints"
        cd <- maybeParser lex constraintParser
        updateState (setConstraints cd))





collect collector parser =
    (parser collector >>= (\x -> collect x parser ))
    <|>
    return collector

actionParser lex precondP effectP = do
    let infoParser = actionInfoParser lex precondP effectP
    try $ T.reserved lex ":action"
    name <- T.identifier lex
    updates <- many infoParser
    let action = foldl (\ a t -> t a) (setName name defaultAction) updates
    updateState (\d -> setItems (domainItem action : getItems d) d)

--    Token.TokenParser a -> CharParser a f -> CharParser a (Record r -> Record r)
actionInfoParser lex precondP effectP =
    paramParser lex
    <|>
    precondParser lex precondP
    <|>
    effectParser lex effectP

effectParser lex condParser = do
    try $ T.reserved lex ":effect"
    effect <- maybeParser lex condParser
    return $ setEffect effect

paramParser lex = do
    try $ T.reserved lex ":parameters"
    params <- T.parens lex $ many $ parseTypedVar lex
    return $ setParameters params

precondParser lex condParser = do
    try $ T.reserved lex ":precondition"
    precond <- maybeParser lex condParser
    return $ setPrecondition precond

