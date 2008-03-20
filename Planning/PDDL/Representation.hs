module Planning.PDDL.Representation (
    Domain(..),
    DomainInfo(..),
    DomainItem(..),
    Condition(..),
    Term(..),
    emptyDomain,
    showType
)where

type Typed a b = (a, Maybe b)
type TypedList a b = [Typed a b ]

showType (a, Nothing) = a
showType (a, Just b) = a ++ " - " ++ b

newtype Domain = Domain (DomainInfo, [DomainItem])

data DomainInfo = DomainInfo {
    domainName :: String,
    requirements :: [String],
    types :: TypedList String String,
    constants :: TypedList String String,
    predicates :: [ (String, TypedList String String) ] -- ^ ( predicate, [(arg-var, arg-type)])
    }
    deriving (Eq)

emptyDomain = Domain (DomainInfo "empty" [] [] [] [], [])

        
showPred (p, tl) = "(" ++ p ++ " " ++ (show tl) ++ ")"

instance Show Domain where
    show (Domain (d, items)) = let indent = "  " in
        "; " ++ (domainName d) ++ " domain\n" ++
        "(define " ++ (show d) ++ "\n" ++
        (unlines $ map show items) ++
        ")\n"

instance Show DomainInfo where
    show d = let indent = "  " in
        "(domain " ++ (domainName d) ++")\n" ++
        "(:requirements" ++ (concatMap (("\n" ++ indent++":")++) $ requirements d) ++ ")\n" ++
        "(:types" ++ (concatMap (\x -> "\n" ++ indent ++ showType x) $ types d) ++ ")\n" ++
        "(:predicates" ++ (concatMap (\pd -> "\n" ++ indent ++ showPred pd) $ 
            predicates d) ++ ")\n"



data DomainItem = 
    Action {
        actionName :: String,
        parameters :: TypedList  String String,
        precondition :: Condition,
        effect :: Condition
    }
    deriving (Eq)

instance Show DomainItem where
    show (Action name params precond effect) = let indent = "  " in
        "(:action " ++ name ++ "\n" ++
        indent ++ ":parameters (" ++ (unwords $ map (\x -> "?" ++ showType x) params) ++ ")\n" ++
        indent ++ ":precondition " ++ (show precond) ++ "\n" ++
        indent ++ ":effect " ++ (show effect) ++ "\n" ++
        ")"

data Condition =
    And [Condition]
    | Atomic String [Term]
    | Empty
    | Exists (TypedList String String) Condition
    | ForAll (TypedList String String) Condition
    | Imply Condition Condition
    | Not Condition
    | Or [Condition]
    | When Condition Condition
    deriving (Eq)
    
instance Show Condition where
    show (And cl) = "(and " ++ (unwords $ map show cl) ++ ")"
    show (Atomic n tl) = "(" ++ (unwords $ n : map show tl) ++ ")"
    show (Exists vars c) = "(exists (" ++ (unwords $ map (\x -> '?' : showType x) vars) ++ ")" ++ (show c) ++ ")"
    show (ForAll vars c) = "(forall (" ++ (unwords $ map (\x -> '?' : showType x) vars) ++ ")" ++ (show c) ++ ")"
    show (Imply c1 c2) = "(implies " ++ (show c1) ++ " " ++ (show c2) ++ ")"
    show (Not c) = "(not " ++ (show c) ++ ")"
    show (Or cl) = "(or " ++ (unwords $ map show cl) ++ ")"
    show Empty = "()"
    show (When c1 c2) = "(when " ++ (show c1) ++ " " ++ (show c2) ++ ")"


data Term =
    Const String
    | Var String
    deriving Eq

instance Show Term where
    show (Const s) = s
    show (Var s) = '?':s


