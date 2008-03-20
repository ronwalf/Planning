module Planning.PDDL.Parser where

import Control.Monad
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Text.ParserCombinators.Parsec.Token as T

import Planning.PDDL.Representation

class (ActionInfoSink c b) => DomainInfoSink a b c | a -> b c where 
    setDomainName   :: String -> a -> a
    setRequirements :: [String] -> a -> a
    setTypes        :: [(String, Maybe String)] -> a -> a
    setConstants    :: [(String, Maybe String)] -> a -> a
    setPredicates   :: [(String, [(String, Maybe String)])] -> a -> a
    setFunctions    :: [((String, [(String, Maybe String)]), Maybe String)] -> a -> a
    setConstraints  :: (Show b) => b -> a -> a
    addAction       :: c -> a -> a

class ActionInfoSink a b | a -> b where
    newAction   :: String -> a
    setParameters   :: [(String, Maybe String)] -> a -> a
    setPrecondition :: b -> a -> a
    setEffect       :: b -> a -> a

class Show a => TermFactory a where
    makeVar         :: String -> a
    makeConst       :: String -> a


class (Show a) => ConditionFactory a b | a -> b where
    makeTrue        :: a
    makeAtomic      :: String -> [b] -> a
    makeNegation    :: a -> a
    makeConjunct    :: [a] -> a
    makeDisjunct    :: [a] -> a
    makeImplies     :: a -> a -> a
    makeWhen        :: a -> a -> a
    makeUniversal   :: [(String, Maybe String)] -> a -> a
    makeExistential :: [(String, Maybe String)] -> a -> a

class (ConditionFactory a b) => PreferenceFactory a b | a -> b where
    makePreference  :: String -> a -> a

instance DomainInfoSink (IO ()) String [String] where
    setDomainName n io = io >> putStrLn ("Domain Name: " ++ n)
    setRequirements r io = io >> putStrLn ("Requirements: " ++ show r)
    setTypes t io = io >> putStrLn ("Types: " ++ show t)
    setConstants c io = io >> putStrLn ("Constants: " ++ show c)
    setPredicates p io = io >> putStrLn ("Predicates: " ++ show p)
    setFunctions f io = io >> putStrLn ("Functions: " ++ show f)
    setConstraints c io = io >> putStrLn("Constraints: " ++ c)
    addAction a io = io >> putStrLn ("Action:" ++ (concatMap (\x -> "   " ++ x ++ "\n") a))

instance ActionInfoSink [String] String where
    newAction = (:[])
    setParameters b a = a ++ ["Parameters: " ++ show b]
    setPrecondition b a = a ++ ["Precondition: " ++ b]
    setEffect b a = a ++ ["Effect: " ++ b]

instance TermFactory String where
    makeVar = ('?':)
    makeConst = id

instance ConditionFactory String String where
    makeTrue = "()"
    makeAtomic  p tl = "(" ++ p ++ (concatMap(' ':) tl) ++ ")"
    makeNegation c = "(not " ++ c ++ ")"
    makeConjunct cl = "(and" ++ (concatMap (' ':) cl) ++ ")"
    makeDisjunct cl = "(or" ++ (concatMap (' ':) cl) ++ ")"
    makeImplies c1 c2 = "(implies " ++ c1 ++ " " ++ c2 ++ ")"
    makeWhen c1 c2 = "(when "++ c1 ++ " " ++ c2 ++ ")"
    makeUniversal vars c = "(forall " ++ (show vars) ++ " " ++ c ++ ")"
    makeExistential vars c = "(exists " ++ (show vars) ++ " " ++ c ++ ")"


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
    Token.reservedNames = ["and", "exists", "forall", "imply", "not", "when", -- Conjunctives
        ":constants", ":constraints", "define", "domain", ":functions", ":predicates", ":requirements", ":types", -- Domain info
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


parseTypedList lex el = many (do
    item <- el
    T.whiteSpace lex
    itemType <- option Nothing (do
        char '-'
        T.whiteSpace lex
        liftM Just $ T.identifier lex
        )
    return (item, itemType)
    )


-- | The domain parser takes a lexer, an domain item parser, and a domain sink
domainParser :: (DomainInfoSink a b c) => Token.TokenParser a -> (GenParser Char a ()) -> (GenParser Char a ()) -> GenParser Char a a
-- domainParser :: Token.TokenParser String -> b
domainParser lex domainInfoParser domainItemParser = T.whiteSpace lex >> T.parens lex (do
    T.reserved lex "define"
    T.parens lex $ (do
        T.reserved lex "domain"
        name <- T.identifier lex
        updateState (setDomainName name)
        )
    many $ T.parens lex (
        domainInfoParser
        <|>
        domainItemParser
        )
    getState
    )

termParser lex = 
    (char '?' >> T.identifier lex >>= (\x -> return $ makeVar x))
    <|>
    (T.identifier lex >>= (\x -> return $ makeConst x))

predicateParser lex =
    (choice [T.reserved lex c >> return c | c <- comparisons])
    <|>
    T.identifier lex
 
atomicFormulaParser lex = do
    name <- predicateParser lex
    arguments <- many $ termParser lex
    return $ makeAtomic name arguments

conditionParser lex = 
    (do
        try $ T.reserved lex "and"
        parts <- many $ T.parens lex $ conditionParser lex
        return $ makeConjunct parts)
    <|>
    (do
        try $ T.reserved lex "or"
        parts <- many $ T.parens lex $ conditionParser lex
        return $ makeDisjunct parts)
    <|>
    (do
        try $ T.reserved lex "not"
        part <- T.parens lex $ conditionParser lex
        return $ makeNegation part)
    <|>
    (do
        try $ T.reserved lex "forall"
        vars <- T.parens lex $ parseTypedList lex (char '?' >> T.identifier lex)
        cond <- T.parens lex $ conditionParser lex
        return $ makeUniversal vars cond)
    <|>
    (do
        try $ T.reserved lex "exists"
        vars <- T.parens lex $ parseTypedList lex (char '?' >> T.identifier lex)
        cond <- T.parens lex $ conditionParser lex
        return $ makeExistential vars cond)
    <|>
    atomicFormulaParser lex


domainInfoParser lex condParser = (
    (do
        try $ T.reserved lex ":requirements"
        ids <- many (char ':' >> T.identifier lex)
        updateState (setRequirements ids))
    <|>
    (do
        try $ T.reserved lex ":types"
        types <- parseTypedList lex $ T.identifier lex
        updateState (setTypes types))
    <|>
    (do
        try $ T.reserved lex ":constants"
        constants <- parseTypedList lex (T.identifier lex)
        updateState (setConstants constants))
    <|>
    (do
        try $ T.reserved lex ":constraints"
        conGD <- condParser
        updateState (setConstraints conGD))
    <|>
    (do
        try $ T.reserved lex ":predicates"
        preds <- many $ T.parens lex (do
            pred <- T.identifier lex
            args <- parseTypedList lex $ (char '?' >> T.identifier lex)
            return (pred, args))
        updateState (setPredicates preds))
    )

collect collector parser =
    (parser collector >>= (\x -> collect x parser ))
    <|>
    return collector

actionInfoParser lex condParser action = 
    (do
        try $ T.reserved lex ":parameters"
        parameters <- T.parens lex $ parseTypedList lex $ (char '?' >> T.identifier lex)
        return $ setParameters parameters action)
    <|>
    (do
        try $ T.reserved lex ":precondition"
        precondition <- T.parens lex condParser
        return $ setPrecondition precondition action)
    <|>
    (do
        try $ T.reserved lex ":effect"
        effect <- T.parens lex condParser
        return $ setEffect effect action)


actionParser lex condParser = do
    try $ T.reserved lex ":action"
    name <- T.identifier lex
    action <- collect (newAction name) (actionInfoParser lex condParser)
    updateState (addAction action)


parseProblem = (T.parens lexer) eof


--standardParser :: (DomainInfoSink a b d, ActionInfoSink d b, ConditionFactory b c, TermFactory c)  => GenParser Char a a
standardParser :: (DomainInfoSink a b c, ActionInfoSink c b, ConditionFactory b d, TermFactory d) => GenParser Char a a
standardParser =
    domainParser lexer (domainInfoParser lexer (conditionParser lexer)) (actionParser lexer (conditionParser lexer))



runPrintingParser parser source input = do
    case (runParser (parser) (print "") source input) of
        Left err -> do
            let msg = "Parse error at " ++ show err
            putStrLn msg
        Right x ->
            x

printingParse = runPrintingParser standardParser

