{-# OPTIONS 
 -fglasgow-exts
 -fallow-undecidable-instances #-}
module Planning.PDDL.Representation (
    module Planning.Expressions,
    module Planning.Records,

    Domain(..),
    emptyDomain, StandardDomain,
    DomainItem(..), domainItem,

    Action(..), defaultAction, StandardAction,

    Problem(..),
    emptyProblem, StandardProblem,

    StdAtomicType,

    GoalExpr,

    PDDLDoc(..),
    pddlExprDoc, docMaybe
) where

import Data.Generics (Data, Typeable, Typeable2)
import Data.List
import Text.PrettyPrint

import Planning.Expressions
import Planning.Records
import qualified Planning.Records as R

-----------------------------
-- Extra Expressions
-----------------------------

type TermExpr = Expr (Const :+: Var)
deriving instance Data TermExpr

type StdAtomicType = Atomic (Expr (Const :+: Var))
--deriving instance Data (Expr StdAtomicType)

type GoalExpr = Expr
    (And :+: Imply :+: Not :+: Or :+:
     (Exists TypedVarExpr) :+: (ForAll TypedVarExpr) :+:
     StdAtomicType)
deriving instance Data GoalExpr


-----------------------------
-- Rendering
-----------------------------
class Functor f => PDDLDoc f where
    pddlDoc :: (PDDLDoc g) => f (Expr g) -> Doc

pddlExprDoc (In x) = pddlDoc x

instance PDDLDoc f => Show (Expr f) where
    show (In f) = show $ pddlDoc f

instance (PDDLDoc f, PDDLDoc g) => PDDLDoc (f :+: g) where
    pddlDoc (Inr x) = pddlDoc x
    pddlDoc (Inl y) = pddlDoc y

instance PDDLDoc Var where
    pddlDoc (Var name) = text ('?':name)

instance PDDLDoc Const where
    pddlDoc (Const name) = text name

instance PDDLDoc t => PDDLDoc (Typed (Expr t)) where
    pddlDoc (Typed (In c) (In t)) =
        (pddlDoc c) <+>
        (char '-') <+>
        (pddlDoc t)

instance PDDLDoc f => PDDLDoc (Atomic (Expr f)) where
    pddlDoc (Atomic p tl) = parens $ hsep $
        (text p) : map (\ (In t) -> pddlDoc t) tl

instance PDDLDoc And where
    pddlDoc (And el) = parens $ sep $ text "and" : [pddlDoc e | In e <- el]

instance PDDLDoc (Exists TypedVarExpr) where
    pddlDoc (Exists vl (In e)) = parens $ sep [
        text "exists",
        parens (sep [ pddlDoc v | In v <- vl ]),
        pddlDoc e ]

instance PDDLDoc (ForAll TypedVarExpr) where
    pddlDoc (ForAll vl (In e)) = parens $ sep [
        text "forall",
        parens (sep [ pddlDoc v | In v <- vl ]),
        pddlDoc e ]
    
instance PDDLDoc Imply where
    pddlDoc (Imply (In e1) (In e2)) = parens $ sep [
        text "implies",
        pddlDoc e1,
        pddlDoc e2]

instance PDDLDoc Not where
    pddlDoc (Not (In e)) = parens $ sep [
        text "not",
        pddlDoc e]

instance PDDLDoc Or where
    pddlDoc (Or el) = parens $ sep $ text "or" : [pddlDoc e | In e <- el]


docNonEmpty :: PDDLDoc f => String -> [Expr f] -> Doc
docNonEmpty name ol =
    if (null ol) then empty else parens $ sep $
        text name :
        [ pddlDoc x | In x <- ol ]

docMaybe :: PDDLDoc f => String -> Maybe (Expr f) -> Doc
docMaybe name Nothing = empty
docMaybe name (Just (In x)) = sep $ [ text name, pddlDoc x ]
------------------------------
-- Domain Description
------------------------------

data Domain a = Domain {
    domainName :: String,
    requirements :: [String],
    -- Types are represented as constants.  Not very consistent, but easily done!
    types :: [TypedConstExpr],
    constants :: [TypedConstExpr],
    predicates :: [ Expr (Atomic TypedVarExpr) ],
    items :: [a]
} deriving (Eq)

{-
type StandardDomain =
    Name :@:
    Requirements :@:
    Types :@:
    (Constants TypedConstExpr) :@:
    (Predicates TypedVarExpr) :@:
    (Items (Action (GoalExpr)))
-}

--type StandardDomain = Domain (Expr (Action (GoalExpr)))
type StandardDomain = Domain (Expr (DomainItem StandardAction))
type StandardAction = Action GoalExpr GoalExpr

emptyDomain = Domain "empty" [] [] [] [] []

instance (PDDLDoc a) => Show (Domain (Expr a)) where
    show domain = show $ parens $ ($$) (text "define") $ vcat $
        parens (text "domain" <+> text (domainName domain)) :
        -- Requirement strings are prefixed with ':'
        parens (sep $ map (text . (':':)) $ "requirements" : requirements domain) :
        parens (sep $ (text ":types") :
            [pddlDoc t | (In t) <- types domain]) :
        parens (sep $ (text ":predicates") :
            [pddlDoc p | (In p) <- predicates domain]) :
        space :
        intersperse space [pddlDoc x | In x <- items domain]

data DomainItem c e = DomainItem c deriving (Data, Eq)
deriving instance Typeable2 DomainItem

instance Functor (DomainItem c) where
    fmap f (DomainItem c) = DomainItem c
instance (Eq c) => FuncEq (DomainItem c) where
    funcEq (DomainItem c1) (DomainItem c2) = c1 == c2
instance PDDLDoc i => PDDLDoc (DomainItem (Expr i)) where
    pddlDoc (DomainItem (In i)) = pddlDoc i
domainItem i = inject $ DomainItem i
------------------------------
-- Action Description
------------------------------
data Action p e = Action Name 
    (Parameters TypedVarExpr)
    (Precondition p)
    (Effect e)
    deriving (Data, Typeable, Show)
instance (Data p, Data e) => HasName (Action p e)
instance (Data p, Data e) => HasParameters TypedVarExpr (Action p e)
instance (Data p, Data e) => HasPrecondition p (Action p e)
instance (Data p, Data e) => HasEffect e (Action p e)
defaultAction :: (Data p, Data e) => Action p e
defaultAction = Action undefined (Parameters []) (Precondition Nothing) (Effect Nothing)

--data Action c e = Action String [TypedVarExpr] (Maybe c) c
{--
    actionName :: String,
    parameters :: [Expr TypedVar],
    precondition :: Maybe GoalExpr,
    effect ::  GoalExpr
--}


instance (Data (Expr p), Data (Expr e), PDDLDoc p, PDDLDoc e) => 
    PDDLDoc (DomainItem (Action (Expr p) (Expr e))) where
    pddlDoc (DomainItem a) = parens $ sep [
        text ":action" <+> (text $ getName a),
        text ":parameters" <+> (parens $ hsep [ pddlDoc v | (In v) <- getParameters a]),
        docMaybe ":precondition" $ getPrecondition a,
        docMaybe ":effect" $ getEffect a]


-------------------------------
-- Problem Description
-------------------------------
data Problem a b c = Problem {
    problemName :: String,
    problemDomain :: String,
    problemRequirements :: [String],
    objects :: [TypedConstExpr],
    initial :: [a],
    goal :: Maybe b,
    constraints :: Maybe c
    }

emptyProblem = Problem {
    problemName = "empty",
    problemDomain = "emptyDomain",
    problemRequirements = [],
    objects = [],
    initial = [],
    goal = Nothing,
    constraints = Nothing
}

type StandardProblem = Problem (Expr (Atomic (Expr Const))) GoalExpr GoalExpr

instance (PDDLDoc a, PDDLDoc b, PDDLDoc c) => Show (Problem (Expr a) (Expr b) (Expr c)) where
    show problem = show $ parens $ sep $
        text "define" :
        (parens $ text "problem" <+> (text $ problemName problem)) :
        (parens $ text ":domain" <+> (text $ problemDomain problem)) :
        (parens $ sep $ text ":requirements" : map (text . (':':)) (problemRequirements problem)) :
        docNonEmpty ":objects" (objects problem) :
        docNonEmpty ":init" (initial problem) :
        maybe empty (\x -> parens $ sep [text ":goal", pddlExprDoc x]) 
            (goal problem) :
        docMaybe ":constraints" (constraints problem) : []

