{-# OPTIONS -XFlexibleInstances #-}
{-# OPTIONS -XTypeOperators #-}
module Planning.PDDL.Representation (
    module Planning.Expressions,

    Domain(..),
    emptyDomain, StandardDomain,
    Action(..), action,

    Typed(..), typed,
    TypedConst,
    TypedConstExpr,
    TypedVar,
    TypedVarExpr,
    Untypeable, removeType,

    StdAtomicType,

    GoalExpr,

    PDDLDoc(..),
    pddlExprDoc
) where

import Data.List
import Text.PrettyPrint

import Planning.Expressions

-----------------------------
-- Extra Expressions
-----------------------------

data Typed t e = Typed t (Expr Const) deriving Eq
instance Functor (Typed t) where
    fmap f (Typed e t) = Typed e t
instance Eq t => FuncEq (Typed t) where
    funcEq (Typed e1 t1) (Typed e2 t2) = (e1 == e2) && (t1 == t2)
typed e t = inject (Typed e t)
type TypedConst = Typed (Expr Const)
type TypedConstExpr = Expr (TypedConst :+: Const)
type TypedVar = Typed (Expr Var)
type TypedVarExpr = Expr (TypedVar :+: Var)

class (Functor f, Functor g) => Untypeable g f where
    untype :: f (Expr g) -> Expr g
instance (Functor h, Untypeable h f, Untypeable h g) => Untypeable h (f :+: g) where
    untype (Inl x) = untype x
    untype (Inr y) = untype y
instance (:<:) Const g => Untypeable g (Typed (Expr Const)) where
    untype (Typed (In (Const c)) _) = eConst c
instance (:<:) Var g => Untypeable g (Typed (Expr Var)) where
    untype (Typed (In (Var v)) _ ) = eVar v
instance (:<:) Const g => Untypeable g Const where
    untype (Const c) = eConst c
instance (:<:) Var g => Untypeable g Var where
    untype (Var v) = eVar v

removeType :: Untypeable g f => Expr f -> Expr g
removeType = foldExpr untype

type StdAtomicType = Atomic (Expr (Const :+: Var))

type GoalExpr = Expr
    (And :+: Imply :+: Not :+: Or :+:
     (Exists TypedVarExpr) :+: (ForAll TypedVarExpr) :+:
     StdAtomicType)


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

type StandardDomain = Domain (Expr (Action (GoalExpr)))

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

------------------------------
-- Action Description
------------------------------
data Action c e = Action String [TypedVarExpr] (Maybe c) c
{--
    actionName :: String,
    parameters :: [Expr TypedVar],
    precondition :: Maybe GoalExpr,
    effect ::  GoalExpr
--}

instance Functor (Action c) where
    fmap f (Action n pl prel el) = Action n pl prel el
instance (Eq c) => FuncEq (Action c) where
    funcEq (Action n1 pl1 pre1 e1) (Action n2 pl2 pre2 e2) =
        (n1 == n2) && (pl1 == pl2) && (pre1 == pre2) && (e1 == e2)
instance PDDLDoc c => PDDLDoc (Action (Expr c)) where
    pddlDoc (Action name params precond (In effect)) = parens $
        (text ":action" <+> (text name)) $$
        (text ":parameters" <+> (parens $ hsep [ pddlDoc v | (In v) <- params ])) $$
        (sep [text ":precondition", (case precond of
            (Just (In cond)) -> pddlDoc cond
            _ -> parens empty)]) $$
        sep [text ":effect", pddlDoc effect]

action name params precond effect= inject (Action name params precond effect)



