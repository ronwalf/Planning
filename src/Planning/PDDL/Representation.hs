{-# LANGUAGE 
    DeriveDataTypeable,
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses,
    OverlappingInstances,
    RankNTypes,
    StandaloneDeriving,
    TypeOperators,
    TypeSynonymInstances,
    UndecidableInstances 
  #-}
module Planning.PDDL.Representation (
    module Planning.Expressions,
    module Planning.Records,

    Domain(..),
    emptyDomain, 
    

    Action(..), defaultAction, 

    Problem(..),
    emptyProblem, 

    PDDLDoc(..),
    PDDLDocExpr(..),
    docMaybe
) where

import Data.Data
import Data.List
import Text.PrettyPrint

import Planning.Expressions
import Planning.Records


-----------------------------
-- Rendering
-----------------------------
class Functor f => PDDLDocExpr f where
    pddlDocExpr :: (PDDLDocExpr g) => f (Expr g) -> Doc

class PDDLDoc d where
    pddlDoc :: d -> Doc
    
instance PDDLDocExpr f => PDDLDoc (Expr f) where
    pddlDoc (In x) = pddlDocExpr x

instance (PDDLDocExpr f, PDDLDocExpr g) => PDDLDocExpr (f :+: g) where
    pddlDocExpr (Inr x) = pddlDocExpr x
    pddlDocExpr (Inl y) = pddlDocExpr y

instance PDDLDocExpr Var where
    pddlDocExpr (Var name) = text ('?':name)

instance PDDLDocExpr Const where
    pddlDocExpr (Const name) = text name

instance PDDLDocExpr Function where
    pddlDocExpr (Function name args) = parens $ sep $
        text name : map pddlDoc args

instance PDDLDocExpr t => PDDLDocExpr (Typed (Expr t)) where
    pddlDocExpr (Typed (In c) (In t)) =
        (pddlDocExpr c) <+>
        (char '-') <+>
        (pddlDocExpr t)

instance PDDLDocExpr f => PDDLDocExpr (Atomic (Expr f)) where
    pddlDocExpr (Atomic p tl) = parens $ hsep $
        (text p) : map (\ (In t) -> pddlDocExpr t) tl

instance PDDLDocExpr And where
    pddlDocExpr (And el) = parens $ sep $ text "and" : [pddlDocExpr e | In e <- el]

instance PDDLDocExpr (Exists TypedVarExpr) where
    pddlDocExpr (Exists vl (In e)) = parens $ sep [
        text "exists",
        parens (sep [ pddlDocExpr v | In v <- vl ]),
        pddlDocExpr e ]

instance PDDLDocExpr (ForAll TypedVarExpr) where
    pddlDocExpr (ForAll vl (In e)) = parens $ sep [
        text "forall",
        parens (sep [ pddlDocExpr v | In v <- vl ]),
        pddlDocExpr e ]
    
instance PDDLDocExpr Imply where
    pddlDocExpr (Imply (In e1) (In e2)) = parens $ sep [
        text "implies",
        pddlDocExpr e1,
        pddlDocExpr e2]

instance PDDLDocExpr Not where
    pddlDocExpr (Not (In e)) = parens $ sep [
        text "not",
        pddlDocExpr e]

instance PDDLDocExpr Or where
    pddlDocExpr (Or el) = parens $ sep $ text "or" : [pddlDocExpr e | In e <- el]


instance PDDLDocExpr p => PDDLDocExpr (When (Expr p)) where
    pddlDocExpr (When p e) = parens $ sep [
        text "when",
        pddlDoc p,
        pddlDoc e]

instance PDDLDocExpr Preference where
    pddlDocExpr (Preference n p) = parens $ sep [
        text "preference",
        maybe empty text n,
        pddlDoc p]

instance PDDLDocExpr Start where
    pddlDocExpr Start = text "start"

instance PDDLDocExpr End where
    pddlDocExpr End = text "end"

instance PDDLDocExpr t => PDDLDocExpr (At (Expr t)) where
    pddlDocExpr (At t e) = parens $ sep [
        text "at",
        pddlDoc t,
        pddlDoc e]

instance PDDLDocExpr t => PDDLDocExpr (Over (Expr t)) where
    pddlDocExpr (Over t e) = parens $ sep [
        text "over",
        pddlDoc t,
        pddlDoc e]

instance PDDLDocExpr Always where
    pddlDocExpr (Always e) = parens $ sep [
        text "always",
        pddlDoc e]

instance PDDLDocExpr Sometime where
    pddlDocExpr (Sometime e) = parens $ sep [
        text "sometime",
        pddlDoc e]

instance PDDLDocExpr SometimeAfter where
    pddlDocExpr (SometimeAfter e1 e2) = parens $ sep [
        text "sometime-after",
        pddlDoc e1,
        pddlDoc e2]

instance PDDLDocExpr SometimeBefore where
    pddlDocExpr (SometimeBefore e1 e2) = parens $ sep [
        text "sometime-before",
        pddlDoc e1,
        pddlDoc e2]

instance PDDLDocExpr Within where
    pddlDocExpr (Within n e) = parens $ sep [
        text "within",
        double n,
        pddlDoc e]

instance PDDLDocExpr AtMostOnce where
    pddlDocExpr (AtMostOnce e) = parens $ sep [
        text "at-most-once",
        pddlDoc e]

instance PDDLDocExpr AlwaysWithin where
    pddlDocExpr (AlwaysWithin n e1 e2) = parens $ sep [
        text "always-within",
        double n,
        pddlDoc e1,
        pddlDoc e2]

instance PDDLDocExpr HoldDuring where
    pddlDocExpr (HoldDuring n1 n2 e) = parens $ sep [
        text "hold-during",
        double n1,
        double n2,
        pddlDoc e]

instance PDDLDocExpr HoldAfter where
    pddlDocExpr (HoldAfter n e) = parens $ sep [
        text "hold-after",
        double n,
        pddlDoc e]

instance PDDLDocExpr OneOf where
    pddlDocExpr (OneOf el) = parens $ sep $
        text "oneof":
        map pddlDoc el

instance PDDLDocExpr Unknown where
    pddlDocExpr (Unknown e) = parens $ sep [
        text "unknown",
        pddlDoc e]


docNonEmpty :: PDDLDocExpr f => String -> [Expr f] -> Doc
docNonEmpty name ol =
    if (null ol) then empty else parens $ sep $
        text name :
        [ pddlDocExpr x | In x <- ol ]

docMaybe :: PDDLDocExpr f => String -> Maybe (Expr f) -> Doc
docMaybe _ Nothing = empty
docMaybe name (Just (In x)) = sep $ [ text name, pddlDocExpr x ]


------------------------------
-- Domain Description
------------------------------
data Domain a b = Domain 
    Name
    Requirements
    (Types TypedConstExpr)
    (Constants TypedConstExpr)
    (Predicates (Expr (Atomic TypedVarExpr)))
    (Functions TypedFuncExpr)
    (Constraints a)
    (Actions b)
    deriving (Data, Eq, Typeable)

instance (Data a, Data b) => HasName (Domain a b)
instance (Data a, Data b) => HasRequirements (Domain a b)
instance (Data a, Data b) => HasTypes TypedConstExpr (Domain a b)
instance (Data a, Data b) => HasConstants TypedConstExpr (Domain a b)
instance (Data a, Data b) => HasPredicates (Expr (Atomic TypedVarExpr)) (Domain a b)
instance (Data a, Data b) => HasFunctions TypedFuncExpr (Domain a b)
instance (Data a, Data b) => HasConstraints a (Domain a b)
instance (Data a, Data b) => HasActions b (Domain a b)

instance (Data (Expr a), Data (Expr b), PDDLDocExpr a, PDDLDocExpr b) => 
    Show (Domain (Expr a) (Expr b)) where
    show domain = show $ parens $ ($$) (text "define") $ vcat $
        parens (text "domain" <+> text (getName domain)) :
         -- Requirement strings are prefixed with ':'
        (if (null $ getRequirements domain) then empty else parens
            (sep $ 
             map (text . (':':)) $ 
             "requirements" : getRequirements domain)) :
        parens (sep $ (text ":types") :
            [pddlDocExpr t | (In t) <- getTypes domain]) :
        parens (sep $ (text ":predicates") :
            [pddlDocExpr p | (In p) <- getPredicates domain]) :
        space :
        intersperse space [pddlDocExpr x | In x <- getActions domain]

emptyDomain :: forall a b. Domain a b
emptyDomain = Domain 
    (Name "empty") 
    (Requirements []) 
    (Types [])
    (Constants [])
    (Predicates [])
    (Functions [])
    (Constraints Nothing)
    (Actions [])

------------------------------
-- Action Description
------------------------------
data Action p e = Action Name 
    (Parameters TypedVarExpr)
    (Precondition p)
    (Effect e)
    deriving (Data, Typeable)
instance (Data p, Data e) => HasName (Action p e)
instance (Data p, Data e) => HasParameters TypedVarExpr (Action p e)
instance (Data p, Data e) => HasPrecondition p (Action p e)
instance (Data p, Data e) => HasEffect e (Action p e)
defaultAction :: (Data p, Data e) => Action p e
defaultAction = Action (Name "empty") (Parameters []) (Precondition Nothing) (Effect Nothing)

instance (Data (Expr p), Data (Expr e), PDDLDocExpr p, PDDLDocExpr e) => 
    PDDLDoc (Action (Expr p) (Expr e)) where
    pddlDoc a = parens $ sep [
        text ":action" <+> (text $ getName a),
        text ":parameters" <+> (parens $ hsep [ pddlDocExpr v | (In v) <- getParameters a]),
        docMaybe ":precondition" $ getPrecondition a,
        docMaybe ":effect" $ getEffect a]


-------------------------------
-- Problem Description
-------------------------------
data Problem a b c = Problem
    Name
    DomainName
    Requirements
    (Constants TypedConstExpr)
    (Initial a)
    (Goal b)
    (Constraints c)
    deriving (Data, Typeable, Eq)

instance (Data a, Data b, Data c) => HasName (Problem a b c)
instance (Data a, Data b, Data c) => HasDomainName (Problem a b c)
instance (Data a, Data b, Data c) => HasRequirements (Problem a b c)
instance (Data a, Data b, Data c) => HasConstants TypedConstExpr (Problem a b c)
instance (Data a, Data b, Data c) => HasInitial a (Problem a b c)
instance (Data a, Data b, Data c) => HasGoal b (Problem a b c)
instance (Data a, Data b, Data c) => HasConstraints c (Problem a b c)

instance 
    (Data (Expr a), Data (Expr b), Data (Expr c),
     PDDLDocExpr a, PDDLDocExpr b, PDDLDocExpr c) =>
    Show (Problem (Expr a) (Expr b) (Expr c)) where
    show prob = show $ parens $ sep $
        text "define" :
        (parens $ text "problem" <+> (text $ getName prob)) :
        (parens $ text ":domain" <+> (text $ getDomainName prob)) :
        (if null $ getRequirements prob then empty else 
           (parens $ sep $ text ":requirements" : map (text . (':':)) (getRequirements prob))) :
        docNonEmpty ":objects" (getConstants prob) :
        docNonEmpty ":init" (getInitial prob) :
        maybe empty (\x -> parens $ sep [text ":goal", pddlDoc x]) 
            (getGoal prob) :
        docMaybe ":constraints" (getConstraints prob) : []

       
    
emptyProblem :: forall a b c. Problem a b c
emptyProblem = Problem
    (Name "empty")
    (DomainName "empty")
    (Requirements [])
    (Constants [])
    (Initial [])
    (Goal Nothing)
    (Constraints Nothing)