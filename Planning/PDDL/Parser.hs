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
        updateState (\d -> d { domainName = name })
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
        updateState (\p -> p { problemName = name })
        )
    many $ T.parens lex probInfoParser
    getState)

constParser :: (:<:) Const t => Token.TokenParser a -> CharParser a (Expr t)
constParser lex = T.identifier lex >>= (\x -> return $ eConst x)
varParser:: (:<:) Var t => Token.TokenParser a -> (CharParser a (Expr t))
varParser lex = char '?' >> T.identifier lex >>= (\x -> return $ eVar x)

termParser :: Token.TokenParser a -> CharParser a (Expr (Const :+: Var))
termParser lex = 
    (varParser lex) <|> (constParser lex)

predicateParser lex =
    (choice [T.reserved lex c >> return c | c <- comparisons])
    <|>
    T.identifier lex

atomicFormulaParser lex argParser = do
    name <- predicateParser lex
    arguments <- many $ argParser 
    return $ eAtomic name arguments

stdStateParser :: forall st .
    T.TokenParser st -> CharParser st (Expr (Atomic (Expr Const)))
stdStateParser lex = 
    T.parens lex $ 
    atomicFormulaParser lex $
    (constParser lex :: CharParser st (Expr Const))

conditionParser :: (
    (:<:) And f,
    (:<:) Or f,
    (:<:) Not f,
    (:<:) (Exists TypedVarExpr) f,
    (:<:) (ForAll TypedVarExpr) f,
    (:<:) StdAtomicType f
    ) => Token.TokenParser a -> CharParser a (Expr f)
conditionParser lex = 
    (do
        try $ T.reserved lex "and"
        parts <- many $ T.parens lex $ conditionParser lex
        return $ eAnd parts)
    <|>
    (do
        try $ T.reserved lex "or"
        parts <- many $ T.parens lex $ conditionParser lex
        return $ eOr parts)
    <|>
    (do
        try $ T.reserved lex "not"
        part <- T.parens lex $ conditionParser lex
        return $ eNot part)
    <|>
    (do
        try $ T.reserved lex "forall"
        vars <- T.parens lex $ many $ parseTypedVar lex
        cond <- conditionParser lex
        return $ eForAll vars cond)
    <|>
    (do
        try $ T.reserved lex "exists"
        vars <- T.parens lex $ many $ parseTypedVar lex
        cond <- conditionParser lex
        return $ eExists vars cond)
    <|>
    atomicFormulaParser lex (termParser lex)

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
        updateState (\d -> d { requirements = ids }))
    <|>
    (do
        try $ T.reserved lex ":types"
        types <- many $ parseTypedConst lex
        updateState (\d -> d { types = types} ))
    <|>
    --(do
    --    try $ T.reserved lex ":constants"
    --    constants <- parseTypedList lex (T.identifier lex)
    --    updateState (\d -> d {constants = constants}))
    -- <|>
    (do
        try $ T.reserved lex ":constraints"
        conGD <- condParser
        return ())
    <|>
    (do
        try $ T.reserved lex ":predicates"
        preds <- many $ T.parens lex (atomicFormulaParser lex (parseTypedVar lex))
        updateState (\d -> d {predicates = preds})
    )

problemInfoParser lex stateParser goalParser constraintParser =
    (do
        try $ T.reserved lex ":domain"
        name <- T.identifier lex
        updateState (\p -> p { problemDomain = name }))
    <|>
    (do
        try $ T.reserved lex ":requirements"
        ids <- many (char ':' >> T.identifier lex)
        updateState (\p -> p { problemRequirements = ids }))
    <|>
    (do
        try $ T.reserved lex ":objects"
        objs <- many $ parseTypedConst lex
        updateState (\d -> d { objects = objs }))
    <|>
    (do
        try $ T.reserved lex ":init"
        model <- many stateParser
        updateState (\p -> p { initial = model }))
    <|>
    (do
        try $ T.reserved lex ":goal"
        gd <- goalParser
        updateState (\p -> p { goal = gd }))
    <|>
    (do
        try $ T.reserved lex ":constraints"
        cd <- constraintParser
        updateState (\p -> p { constraints = cd }))





collect collector parser =
    (parser collector >>= (\x -> collect x parser ))
    <|>
    return collector

actionParser lex condParser = do
    let infoParser = actionInfoParser lex condParser
    try $ T.reserved lex ":action"
    name <- T.identifier lex
    updates <- many infoParser
    let action = foldl (\ a t -> t a) (setName name defaultAction) updates
    updateState (\d -> d { items = domainItem action : items d })

--    Token.TokenParser a -> CharParser a f -> CharParser a (Record r -> Record r)
actionInfoParser lex condParser =
    paramParser lex
    <|>
    precondParser lex condParser
    <|>
    (do
        try $ T.reserved lex ":effect"
        effect <- maybeParser lex condParser
        return $ setEffect effect)

paramParser lex = do
    try $ T.reserved lex ":parameters"
    params <- T.parens lex $ many $ parseTypedVar lex
    return $ setParameters params

precondParser lex condParser = do
    try $ T.reserved lex ":precondition"
    precond <- maybeParser lex condParser
    return $ setPrecondition precond


parseProblem = (T.parens lexer) eof


standardParser :: GenParser Char StandardDomain StandardDomain
standardParser =
    let condParser = conditionParser lexer :: CharParser StandardDomain GoalExpr in
    domainParser lexer 
        (domainInfoParser lexer condParser)
        (actionParser lexer condParser)


stdProblemParser :: GenParser Char StandardProblem StandardProblem
stdProblemParser =
    problemParser lexer $
    problemInfoParser lexer
    (stdStateParser lexer :: CharParser StandardProblem (Expr (Atomic (Expr Const))))
    (maybeParser lexer $ conditionParser lexer)
    (maybeParser lexer $ conditionParser lexer)
        

runPrintingParser parser source input = do
    case (runParser (parser) (print "") source input) of
        Left err -> do
            let msg = "Parse error at " ++ show err
            putStrLn msg
        Right x ->
            print x
