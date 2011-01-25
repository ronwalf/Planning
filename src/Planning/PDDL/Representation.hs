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
    
    DomainItem(..), domainItem, maybeItem,

    Action(..), defaultAction, 

    Problem(..),
    emptyProblem, 

    PDDLDoc(..),
    pddlExprDoc, docMaybe
) where

import Data.Data
import Data.List
import Text.PrettyPrint

import Planning.Expressions
import Planning.Records


-----------------------------
-- Rendering
-----------------------------
class Functor f => PDDLDoc f where
    pddlDoc :: (PDDLDoc g) => f (Expr g) -> Doc

pddlExprDoc :: PDDLDoc f => Expr f -> Doc
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

instance PDDLDoc Function where
    pddlDoc (Function name args) = parens $ sep $
        text name : map pddlExprDoc args

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


instance PDDLDoc p => PDDLDoc (When (Expr p)) where
    pddlDoc (When p e) = parens $ sep [
        text "when",
        pddlExprDoc p,
        pddlExprDoc e]

instance PDDLDoc Preference where
    pddlDoc (Preference n p) = parens $ sep [
        text "preference",
        maybe empty text n,
        pddlExprDoc p]

instance PDDLDoc Start where
    pddlDoc Start = text "start"

instance PDDLDoc End where
    pddlDoc End = text "end"

instance PDDLDoc t => PDDLDoc (At (Expr t)) where
    pddlDoc (At t e) = parens $ sep [
        text "at",
        pddlExprDoc t,
        pddlExprDoc e]

instance PDDLDoc t => PDDLDoc (Over (Expr t)) where
    pddlDoc (Over t e) = parens $ sep [
        text "over",
        pddlExprDoc t,
        pddlExprDoc e]

instance PDDLDoc Always where
    pddlDoc (Always e) = parens $ sep [
        text "always",
        pddlExprDoc e]

instance PDDLDoc Sometime where
    pddlDoc (Sometime e) = parens $ sep [
        text "sometime",
        pddlExprDoc e]

instance PDDLDoc SometimeAfter where
    pddlDoc (SometimeAfter e1 e2) = parens $ sep [
        text "sometime-after",
        pddlExprDoc e1,
        pddlExprDoc e2]

instance PDDLDoc SometimeBefore where
    pddlDoc (SometimeBefore e1 e2) = parens $ sep [
        text "sometime-before",
        pddlExprDoc e1,
        pddlExprDoc e2]

instance PDDLDoc Within where
    pddlDoc (Within n e) = parens $ sep [
        text "within",
        double n,
        pddlExprDoc e]

instance PDDLDoc AtMostOnce where
    pddlDoc (AtMostOnce e) = parens $ sep [
        text "at-most-once",
        pddlExprDoc e]

instance PDDLDoc AlwaysWithin where
    pddlDoc (AlwaysWithin n e1 e2) = parens $ sep [
        text "always-within",
        double n,
        pddlExprDoc e1,
        pddlExprDoc e2]

instance PDDLDoc HoldDuring where
    pddlDoc (HoldDuring n1 n2 e) = parens $ sep [
        text "hold-during",
        double n1,
        double n2,
        pddlExprDoc e]

instance PDDLDoc HoldAfter where
    pddlDoc (HoldAfter n e) = parens $ sep [
        text "hold-after",
        double n,
        pddlExprDoc e]

instance PDDLDoc OneOf where
    pddlDoc (OneOf el) = parens $ sep $
        text "oneof":
        map pddlExprDoc el

instance PDDLDoc Unknown where
    pddlDoc (Unknown e) = parens $ sep [
        text "unknown",
        pddlExprDoc e]


docNonEmpty :: PDDLDoc f => String -> [Expr f] -> Doc
docNonEmpty name ol =
    if (null ol) then empty else parens $ sep $
        text name :
        [ pddlDoc x | In x <- ol ]

docMaybe :: PDDLDoc f => String -> Maybe (Expr f) -> Doc
docMaybe _ Nothing = empty
docMaybe name (Just (In x)) = sep $ [ text name, pddlDoc x ]


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
    (Items b)
    deriving (Data, Eq, Typeable)

instance (Data a, Data b) => HasName (Domain a b)
instance (Data a, Data b) => HasRequirements (Domain a b)
instance (Data a, Data b) => HasTypes TypedConstExpr (Domain a b)
instance (Data a, Data b) => HasConstants TypedConstExpr (Domain a b)
instance (Data a, Data b) => HasPredicates (Expr (Atomic TypedVarExpr)) (Domain a b)
instance (Data a, Data b) => HasFunctions TypedFuncExpr (Domain a b)
instance (Data a, Data b) => HasConstraints a (Domain a b)
instance (Data a, Data b) => HasItems b (Domain a b)

instance (Data (Expr a), Data (Expr b), PDDLDoc a, PDDLDoc b) => 
    Show (Domain (Expr a) (Expr b)) where
    show domain = show $ parens $ ($$) (text "define") $ vcat $
        parens (text "domain" <+> text (getName domain)) :
         -- Requirement strings are prefixed with ':'
        (if (null $ getRequirements domain) then empty else parens
            (sep $ 
             map (text . (':':)) $ 
             "requirements" : getRequirements domain)) :
        parens (sep $ (text ":types") :
            [pddlDoc t | (In t) <- getTypes domain]) :
        parens (sep $ (text ":predicates") :
            [pddlDoc p | (In p) <- getPredicates domain]) :
        space :
        intersperse space [pddlDoc x | In x <- getItems domain]

emptyDomain :: forall a b. Domain a b
emptyDomain = Domain 
    (Name "empty") 
    (Requirements []) 
    (Types [])
    (Constants [])
    (Predicates [])
    (Functions [])
    (Constraints Nothing)
    (Items [])

data DomainItem c e = DomainItem c deriving (Data, Eq)
deriving instance Typeable2 DomainItem

instance Functor (DomainItem c) where
    fmap _ (DomainItem c) = DomainItem c
instance (Eq c) => FuncEq (DomainItem c) where
    funcEq (DomainItem c1) (DomainItem c2) = c1 == c2
instance PDDLDoc i => PDDLDoc (DomainItem (Expr i)) where
    pddlDoc (DomainItem (In i)) = pddlDoc i
domainItem :: (:<:) (DomainItem c) f => c -> Expr f
domainItem i = inject $ DomainItem i

class (Functor f) => MaybeItem f c where
    maybeItem' :: f (Maybe c) -> Maybe c

maybeItem :: (MaybeItem f c) => Expr f -> Maybe c
maybeItem = foldExpr maybeItem'

instance (MaybeItem f c, MaybeItem g c) => MaybeItem (f :+: g) c where
    maybeItem' (Inr x) = maybeItem' x
    maybeItem' (Inl x) = maybeItem' x
{-
instance (DomainItem c :<: f, Functor g) => MaybeItem (f :+: g) c where
    maybeItem' (Inr x) = maybeItem' x
    maybeItem' _ = Nothing

instance (DomainItem c :<: g, Functor f) => MaybeItem (f :+: g) c where
    maybeItem' (Inr x) = maybeItem' x
    maybeItem' _ = Nothing
-}
instance MaybeItem (DomainItem c) c where
    maybeItem' (DomainItem c) = Just c

instance MaybeItem (DomainItem d) c where
    maybeItem' _ = Nothing

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
defaultAction = Action (Name "empty") (Parameters []) (Precondition Nothing) (Effect Nothing)

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
     PDDLDoc a, PDDLDoc b, PDDLDoc c) =>
    Show (Problem (Expr a) (Expr b) (Expr c)) where
    show prob = show $ parens $ sep $
        text "define" :
        (parens $ text "problem" <+> (text $ getName prob)) :
        (parens $ text ":domain" <+> (text $ getDomainName prob)) :
        (if null $ getRequirements prob then empty else 
           (parens $ sep $ text ":requirements" : map (text . (':':)) (getRequirements prob))) :
        docNonEmpty ":objects" (getConstants prob) :
        docNonEmpty ":init" (getInitial prob) :
        maybe empty (\x -> parens $ sep [text ":goal", pddlExprDoc x]) 
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
