{-# LANGUAGE
    FlexibleContexts,
    RankNTypes,
    ScopedTypeVariables,
    TypeOperators
  #-}
module Planning.PDDL.Parser (
    pddlDescLanguage,
    pddlExprLanguage,
    pddlDescLexer,
    pddlExprLexer,
    parseTypedList,
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
    atomicTypeParser,
    andParser,
    andListParser,
    orParser,
    notParser,
    implyParser,
    whenParser,
    conditionalParser,
    forallParser,
    universalParser,
    existsParser,
    preferenceParser,
    prefListParser,
    oneOfParser,
    unknownParser,
    emptyOrParser,
    emptyAndListParser,
    maybeParser,
    derivedParser,
    actionParser,
    actionInfoParser,
    effectParser,
    paramParser,
    precondParser,
    collect
) where

import Control.Monad (liftM)
import Data.Data
import Data.Function (on)
import Data.List (deleteFirstsBy)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P

import Planning.PDDL.Representation

comparisons :: [String]
comparisons = ["-", "/", "*", "<", ">", "=", ">=", "<="]

baseLanguage :: P.LanguageDef st
baseLanguage = P.LanguageDef {
    P.commentStart = "",
    P.commentEnd = "",
    P.commentLine = ";",
    P.nestedComments = False,
    P.identStart = letter,
    P.identLetter = alphaNum <|> oneOf "_-",
    P.opStart = oneOf [],
    P.opLetter = oneOf [],
    P.reservedNames = [],
    P.reservedOpNames = [],
    P.caseSensitive = False
}

pddlDescLanguage :: P.LanguageDef st
pddlDescLanguage = baseLanguage
    {- -- Reserved names are always in syntactically distinct locations from identifiers
       -- ... meaning we don't need to reserve them.
    {
    P.reservedNames = [
        "-", -- Types
        ":constants", ":constraints", "define", "domain", ":functions", ":predicates", ":requirements", ":types", -- Domain info
        ":derived", -- Derived predicates
        ":objects", "problem", ":domain",-- Problem info
        ":action", ":effect", ":parameters", ":precondition" -- Action info
        ]
    }
    -}

pddlExprLanguage :: P.LanguageDef st
pddlExprLanguage = baseLanguage {
    P.reservedNames = [
        "and", "exists", "forall", "imply", "not", "when" -- Conditions
        ]
        ++ comparisons
}
pddlDescLexer :: P.TokenParser a
pddlDescLexer = P.makeTokenParser pddlDescLanguage

pddlExprLexer :: P.TokenParser a
pddlExprLexer = P.makeTokenParser pddlExprLanguage

parseTypedList :: forall a c . P.TokenParser a -> CharParser a c -> CharParser a [Expr (Typed c)]
parseTypedList mylex cparser = liftM concat $ many $ do
    terms <- many1 cparser
    tl <- option [] $ (>>) (try $ P.symbol mylex "-") $ (
        try (tparser >>= return . (:[]))
        <|>
        try (P.parens mylex $ do
            P.reserved mylex "either"
            many tparser))
    return $ map (flip eTyped (map T.pack $ catMaybes tl) :: c -> Expr (Typed c)) terms
    where
        tparser =
            (do
                P.reserved mylex "object"
                return Nothing)
            <|>
            (do
                t <- P.identifier mylex
                return (Just t))


{-
parseTypedConst :: P.TokenParser a -> CharParser a TypedConstExpr
parseTypedConst mylex = do
    In (Const cstr) <- constParser mylex
    option (eConst cstr) (do
        P.reserved mylex "-"
        tstr <- P.identifier mylex
        return $ eTyped (eConst cstr :: Expr Const) (eConst tstr :: Expr Const))


parseTypedVar :: P.TokenParser a -> CharParser a TypedVarExpr
parseTypedVar mylex = do
    In (Var vstr) <- varParser mylex
    option (eVar vstr) (do
        P.reserved mylex "-"
        tstr <- P.identifier mylex
        return $ eTyped (eVar vstr :: Expr Var) (eConst tstr :: Expr Const))
-}

-- | The domain parser takes a pddlLexer, an domain item parser, and a domain sink
domainParser :: (HasName b, HasActions a b, HasDerived d b) =>
    P.TokenParser b
    -> CharParser b ()
    -> CharParser b b
domainParser mylex infoParser = P.whiteSpace mylex >> P.parens mylex (do
    P.reserved mylex "define"
    P.parens mylex (do
        P.reserved mylex "domain"
        name <- P.identifier mylex
        updateState (setName $ T.pack name)
        )
    skipMany $ P.parens mylex infoParser
    updateState (\d ->
        setDerived (reverse $ getDerived d) $
        setActions (reverse $ getActions d) d)
    getState
    )

problemParser :: (HasName b) =>
    P.TokenParser b
    -> CharParser b a
    -> CharParser b b
problemParser mylex probInfoParser = P.whiteSpace mylex >> P.parens mylex (do
    P.reserved mylex "define"
    P.parens mylex (do
        P.reserved mylex "problem"
        name <- P.identifier mylex
        updateState (setName $ T.pack name)
        )
    skipMany $ P.parens mylex probInfoParser
    getState)

constParser :: (:<:) Const t => P.TokenParser a -> CharParser a (Expr t)
constParser mylex = P.identifier mylex >>= (return . eConst . T.pack)

varParser :: (:<:) Var t => P.TokenParser a -> CharParser a (Expr t)
varParser mylex = char '?' >> P.identifier mylex >>= (return . eVar . T.pack)

functionParser :: (:<:) Function t =>
    P.TokenParser a -> CharParser a (Expr t) -> CharParser a (Expr t)
functionParser mylex tp = P.parens mylex $ do
    name <- P.identifier mylex
    args <- many tp
    return $ eFunc (T.pack name) args

--termParser :: P.TokenParser a -> CharParser a TermExpr
--termParser mylex =
--    (varParser mylex) <|> (constParser mylex) <|> (functionParser mylex $ termParser mylex)
predicateParser :: P.TokenParser st -> CharParser st String
predicateParser mylex =
    (choice [P.reserved mylex c >> return c | c <- comparisons])
    <|>
    P.identifier mylex

stdStateParser :: (Atomic a :<: f) =>
    P.TokenParser st
    -> CharParser st a
    -> CharParser st (Expr f)

stdStateParser mylex termP =
    P.parens mylex $
    atomicParser mylex termP

atomicParser :: (Atomic a :<: f) =>
    P.TokenParser st
    -> CharParser st a
    -> CharParser st (Expr f)
atomicParser mylex termP = do
    name <- predicateParser mylex
    arguments <- many $ termP
    return $ eAtomic (T.pack name) arguments

atomicTypeParser :: P.TokenParser st
    -> CharParser st a
    -> CharParser st (Expr (Atomic (Expr (Typed a))))
atomicTypeParser mylex termP = do
    name <- predicateParser mylex
    arguments <- parseTypedList mylex termP
    return $ eAtomic (T.pack name) arguments


andParser :: (And :<: f) =>
    P.TokenParser st
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
andParser mylex exprP = do
    try $ P.reserved mylex "and"
    parts <- many $ P.parens mylex exprP
    return $ eAnd parts

{-|
  andListParserparses a potentially( list (innerP) expressions which
  If 'innerP' parses expressions of the form <inner>, then 'andListParser'
  parses expressions of the form:

   * <inner>
   * and (inner) ... (inner)
   * and (and (inner) ... (inner)) (inner)

   and so forth.  Returns a flat list of return values of 'innerP'

-}


andListParser ::
    P.TokenParser st
    -> CharParser st f
    -> CharParser st [f]
andListParser mylex innerP = do
    andP <|> partP
    where
    andP = do
        try $ P.reserved mylex "and"
        parts <- many $ P.parens mylex (andListParser mylex innerP)
        return $ concat parts
    partP = do
        p <- innerP
        return [p]

orParser :: (Or :<: f) =>
    P.TokenParser st
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
orParser mylex exprP = do
    try $ P.reserved mylex "or"
    parts <- many $ P.parens mylex exprP
    return $ eOr parts

notParser :: (Not :<: f) =>
    P.TokenParser st
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
notParser mylex exprP = do
    try $ P.reserved mylex "not"
    part <- P.parens mylex exprP
    return $ eNot part

implyParser :: (Imply :<: f) =>
    P.TokenParser st
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
implyParser mylex exprP = do
    try $ P.reserved mylex "imply"
    cond <- P.parens mylex exprP
    res <- P.parens mylex exprP
    return $ eImply cond res

whenParser :: (When a :<: f) =>
    P.TokenParser st
    -> CharParser st a
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
whenParser mylex condP exprP = do
    try $ P.reserved mylex "when"
    cond <- P.parens mylex condP
    eff <- P.parens mylex exprP
    return $ eWhen cond eff

{-|
  If 'condP' and 'innerP' parse expressions of the form <cond> and <inner>
  respetively, then 'conditionalParser' parses expressions of the form:

  * <inner>
  * when (<cond>) (<inner>)
  * and (<inner>) (when (<cond>) (<inner>))

  and so forth.  Returns the concatenated results of 'innerP'.

-}
conditionalParser :: forall st p e .
    P.TokenParser st
    -> CharParser st p
    -> (Maybe p -> CharParser st [e])
    -> CharParser st [e]
conditionalParser mylex condP innerP =
    liftM concat $
    andListParser mylex (whenP <|> partP)
    where
    whenP :: CharParser st [e]
    whenP = do
        try $ P.reserved mylex "when"
        cond <- P.parens mylex condP
        P.parens mylex $
            innerP $
            Just cond
    partP :: CharParser st [e]
    partP = innerP Nothing

forallParser :: (ForAll TypedVarExpr :<: f) =>
    P.TokenParser st
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
forallParser mylex exprP = do
    try $ P.reserved mylex "forall"
    vars <- P.parens mylex $ parseTypedList mylex $ varParser mylex
    cond <- P.parens mylex exprP
    return $ eForAll (vars :: [TypedVarExpr]) cond

{-|
  If 'innerP' takes a list of 'TypedVarExpr' and parses expressions
  of the form <inner>, then 'innerP' parses expressions of the form:

  * <inner>
  * forall (typed var list) (<inner>)
  * and (inner) (forall (typed var list) (<inner>)) ...
  * forall (tvar list) (forall (tvar list) (<inner>))

  and so forth.  'universalParser' concantenates and returns the return
  values of 'innerP'.  The var list passed to 'innerP' will only
  have the relevant variables, not ones that have been replaced by inner bindings.
-}
universalParser ::
    P.TokenParser st
    -> ([TypedVarExpr] -> CharParser st [a])
    -> CharParser st [a]
universalParser mylex innerP = do
    universalP' []
    where
    universalP' vars = liftM concat $
        andListParser mylex (forallP vars <|> innerP vars)
    newVars vars vars' =
        vars' ++
        deleteFirstsBy ((==) `on` removeType) vars vars'
    forallP vars = do
        try $ P.reserved mylex "forall"
        vars' <- P.parens mylex $ parseTypedList mylex $ varParser mylex
        P.parens mylex $ universalP' $ newVars vars vars'


existsParser :: (Exists TypedVarExpr :<: f) =>
    P.TokenParser st
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
existsParser mylex exprP = do
    try $ P.reserved mylex "exists"
    vars <- P.parens mylex $ parseTypedList mylex $ varParser mylex
    cond <- P.parens mylex exprP
    return $ eExists (vars :: [TypedVarExpr]) cond

preferenceParser :: (Preference :<: f) =>
    P.TokenParser st
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
preferenceParser mylex exprP = do
    try $ P.reserved mylex "preference"
    name <- optionMaybe (P.identifier mylex)
    expr <- P.parens mylex exprP
    return $ ePreference (liftM T.pack name) expr


{-|
  If 'innerP' parses expressions of the form 'inner',
  'prefListParser' parses expressions of the form:

  * <inner>
  * preference <name> <inner>
  * and (inner) ... (inner)
  * and (preference <name> <inner>) ... (inner)
  * and (and ...) (inner)

  and so forth.  Returns a list of 'innerP' return values,
  annotated by the preference name, if it existed.
-}

prefListParser ::
    P.TokenParser st
    -> CharParser st f
    -> CharParser st [(Maybe T.Text, f)]
prefListParser mylex innerP = do
    andListParser mylex (prefP <|> partP)
    where
    nameP = (try $ P.identifier mylex) <|> (return "")
    prefP = do
        try $ P.reserved mylex "preference"
        name <- nameP
        inner <- innerP
        return (Just $ T.pack name, inner)
    partP = do
        inner <- innerP
        return (Nothing, inner)

oneOfParser :: (OneOf :<: f) =>
    P.TokenParser st
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
oneOfParser mylex exprP = do
    try $ P.reserved mylex "oneOf"
    parts <- many1 $ P.parens mylex exprP
    return $ eOneOf parts

unknownParser :: (Unknown :<: f) =>
    P.TokenParser st
    -> CharParser st (Expr f)
    -> CharParser st (Expr f)
unknownParser mylex exprP = do
    try $ P.reserved mylex "unknown"
    part <- P.parens mylex exprP
    return $ eUnknown part

{-|
If 'p' parses expressions of the form <p> and returns a list,
'emptyOrParser' parses expressions of the form:

 * ()
 * (<p>)

Returns [] for '()', otherwise returns the value from 'p'
-}
emptyOrParser :: P.TokenParser st
    -> CharParser st [a]
    -> CharParser st [a]
emptyOrParser mylex p =
    P.parens mylex $
    option [] p

{-|
If 'p' parses expressions of the form <p> and returns a value,
'emptyOrParser' parses expressions of the form:

 * ()
 * (<p>)
 * (and <p>*)

Returns [] for '()', otherwise returns the value from 'p'
-}
emptyAndListParser ::
    P.TokenParser st
    -> CharParser st f
    -> CharParser st [f]
emptyAndListParser mylex innerP =
    option [] $ try $ andListParser mylex innerP

maybeParser :: P.TokenParser st
    -> CharParser st a
    -> CharParser st (Maybe a)
maybeParser mylex p =
    (do
        try $ P.parens mylex $ P.whiteSpace mylex
        return Nothing)
    <|>
    (do
        result <- P.parens mylex p
        return $ Just result)

domainInfoParser :: forall st a .
    (HasRequirements st,
        HasTypes (Expr (Typed T.Text)) st,
        HasConstants TypedConstExpr st,
        HasConstraints a st,
        HasPredicates TypedPredicateExpr st) =>
    P.TokenParser st
    -> CharParser st a
    -> CharParser st ()
domainInfoParser mylex condParser =
    (do
        try $ P.reserved mylex ":requirements"
        ids <- many (char ':' >> P.identifier mylex)
        updateState (setRequirements $ map T.pack ids))
    <|>
    (do
        try $ P.reserved mylex ":types"
        types <- parseTypedList mylex $ liftM T.pack $ P.identifier mylex
        updateState (setTypes types))
    <|>
    (do
        try $ P.reserved mylex ":constants"
        constants <- parseTypedList mylex $ constParser mylex
        updateState (setConstants constants))
     <|>
    (do
        try $ P.reserved mylex ":constraints"
        conGD <- emptyAndListParser mylex condParser
        updateState (setConstraints conGD))
    <|>
    (do
        try $ P.reserved mylex ":predicates"
        preds <- many $ P.parens mylex (atomicTypeParser mylex (varParser mylex) :: CharParser st TypedPredicateExpr)
        updateState (setPredicates preds)
    )

problemInfoParser :: (HasDomainName st,
        HasRequirements st,
        HasConstants TypedConstExpr st,
        HasInitial a st,
        HasGoal a1 st,
        HasConstraints a2 st) =>
        P.TokenParser st
    -> GenParser Char st a
    -> CharParser st a1
    -> CharParser st a2
    -> CharParser st ()
problemInfoParser mylex stateParser goalParser constraintParser =
    (do
        try $ P.reserved mylex ":domain"
        name <- P.identifier mylex
        updateState (setDomainName $ T.pack name))
    <|>
    (do
        try $ P.reserved mylex ":requirements"
        ids <- many (char ':' >> P.identifier mylex)
        updateState (setRequirements $ map T.pack ids))
    <|>
    (do
        try $ P.reserved mylex ":objects"
        objs <- parseTypedList mylex $ constParser mylex
        updateState (setConstants objs))
    <|>
    (do
        try $ P.reserved mylex ":init"
        model <- many stateParser
        updateState (setInitial model))
    <|>
    (do
        try $ P.reserved mylex ":goal"
        gd <- maybeParser mylex goalParser
        updateState (setGoal gd))
    <|>
    (do
        try $ P.reserved mylex ":constraints"
        cd <- emptyAndListParser mylex constraintParser
        updateState (setConstraints cd))

collect ::
    a -> (a -> GenParser tok st a) -> GenParser tok st a
collect collector parser =
    (parser collector >>= (\x -> collect x parser ))
    <|>
    return collector

derivedParser :: (Data p, Data g,
        HasDerived (p, g) st) =>
    P.TokenParser st
    -> CharParser st p
    -> CharParser st g
    -> CharParser st ()
derivedParser mylex headP bodyP = do
    try $ P.reserved mylex ":derived"
    h <- P.parens mylex $ headP
    b <- P.parens mylex $ bodyP
    updateState (\d -> setDerived ((h, b) : getDerived d) d)

actionParser :: (Data p,
        Data e,
        HasActions (Action p e) st) =>
    P.TokenParser st
    -> CharParser st [p]
    -> CharParser st [e]
    -> CharParser st ()
actionParser mylex precondP effectP = do
    let infoParser = actionInfoParser mylex precondP effectP
    try $ P.reserved mylex ":action"
    name <- P.identifier mylex
    updates <- many infoParser
    let action = foldl (\ a t -> t a) (setName (T.pack name) defaultAction) updates
    updateState (\d -> setActions (action : getActions d) d)

actionInfoParser :: (HasParameters TypedVarExpr a,
        HasPrecondition a1 a,
        HasEffect a2 a) =>
    P.TokenParser st
    -> CharParser st [a1]
    -> CharParser st [a2]
    -> CharParser st (a -> a)
actionInfoParser mylex precondP effectP =
    paramParser mylex
    <|>
    precondParser mylex precondP
    <|>
    effectParser mylex effectP

effectParser :: (HasEffect a a1) =>
        P.TokenParser st
        -> CharParser st [a]
        -> CharParser st (a1 -> a1)
effectParser mylex condParser = do
    try $ P.reserved mylex ":effect"
    effect <- emptyOrParser mylex condParser
    return $ setEffect effect

paramParser :: (HasParameters TypedVarExpr a) =>
    P.TokenParser st -> CharParser st (a -> a)
paramParser mylex = do
    try $ P.reserved mylex ":parameters"
    params <- P.parens mylex $ parseTypedList mylex $ varParser mylex
    return $ setParameters params

precondParser :: (HasPrecondition a a1) =>
    P.TokenParser st
    -> CharParser st [a]
    -> CharParser st (a1 -> a1)
precondParser mylex condParser = do
    try $ P.reserved mylex ":precondition"
    precond <- emptyOrParser mylex condParser
    return $ setPrecondition precond
