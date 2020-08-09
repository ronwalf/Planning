{-# LANGUAGE
    DeriveDataTypeable,
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses,
    RankNTypes,
    OverloadedStrings,
    ScopedTypeVariables,
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
    docList, docMaybe, docNonEmpty, docAnd
) where

import Data.Data
import Data.Function (on)
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc

import Planning.Expressions
import Planning.Records


-- Utilities
prettyT :: Text -> Doc ann
prettyT = pretty

-----------------------------
-- Rendering
-----------------------------
class Functor f => PDDLDocExpr f where
    pddlDocExpr :: (PDDLDocExpr g) => f (Expr g) -> Doc ann

class PDDLDoc d where
    pddlDoc :: d -> Doc ann

instance PDDLDoc Text where
    pddlDoc = pretty

instance PDDLDocExpr f => PDDLDoc (Expr f) where
    pddlDoc (In x) = pddlDocExpr x

instance {-# OVERLAPS #-} (PDDLDocExpr f, PDDLDocExpr g) => PDDLDocExpr (f :+: g) where
    {-# NOINLINE pddlDocExpr #-}
    pddlDocExpr (Inr x) = pddlDocExpr x
    pddlDocExpr (Inl y) = pddlDocExpr y

instance PDDLDocExpr Var where
    pddlDocExpr (Var name) = pretty ('?' `T.cons` name)

instance PDDLDocExpr Const where
    pddlDocExpr (Const name) = pretty name

instance PDDLDocExpr Function where
    pddlDocExpr (Function name args) = parens $ sep $
        pretty name : map pddlDoc args

instance PDDLDoc t => PDDLDoc [Expr (Typed t)] where
    pddlDoc tl =
        let
            groups :: [[Expr (Typed t)]]
            groups = groupBy ((==) `on` getType) tl
        in
        sep $ fst $ foldr typelistDoc ([], []) groups
        where

        typelistDoc :: [Expr (Typed t)] -> ([Doc ann], [Text]) -> ([Doc ann], [Text])
        typelistDoc [] x = x
        typelistDoc tg@(h:_) (doc, ptype)
            | getType h == ptype
                = (sep (docSameTypes tg) : doc, ptype)
            | otherwise
                = let dt = prettyT "-" <+> docType (getType h) in
                    (sep (docSameTypes tg ++ [dt]) : doc, getType h)
        docSameTypes :: [Expr (Typed t)] -> [Doc ann]
        docSameTypes = map (pddlDoc . removeType)
        docType :: [Text] -> Doc ann
        docType [] = prettyT "object"
        docType [t] = pretty t
        docType l = parens $ sep $ prettyT "either" : map pretty l

-- No PDDLDocExpr for Typed, which gives us a way to distinquish
-- typed instances and render them differently.
instance {-# OVERLAPS #-} PDDLDocExpr t => PDDLDocExpr (Atomic (Expr t)) where
    pddlDocExpr (Atomic p tl) = parens $ hsep $
        (pretty p) : map pddlDoc tl

instance {-# OVERLAPS #-} PDDLDoc t => PDDLDocExpr (Atomic (Expr (Typed t))) where
    pddlDocExpr (Atomic p tl) = parens $
        (pretty p) <+> pddlDoc tl

instance PDDLDocExpr And where
    pddlDocExpr (And el) = parens $ sep $ prettyT "and" : [pddlDocExpr e | In e <- el]

instance PDDLDocExpr (Exists TypedVarExpr) where
    pddlDocExpr (Exists vl e) = parens $ sep [
        prettyT "exists",
        parens (pddlDoc vl),
        pddlDoc e ]

instance PDDLDocExpr (ForAll TypedVarExpr) where
    pddlDocExpr (ForAll vl e) = parens $ sep [
        prettyT "forall",
        parens (pddlDoc vl),
        pddlDoc e ]

instance PDDLDocExpr Imply where
    pddlDocExpr (Imply e1 e2) = parens $ sep [
        prettyT "implies",
        pddlDoc e1,
        pddlDoc e2]

instance PDDLDocExpr Not where
    pddlDocExpr (Not e) = parens $ sep [
        prettyT "not",
        pddlDoc e]

instance PDDLDocExpr Or where
    pddlDocExpr (Or el) = parens $ sep $ prettyT "or" : [pddlDocExpr e | In e <- el]


instance {-# OVERLAPS #-} PDDLDoc p => PDDLDocExpr (When p) where
    pddlDocExpr (When p e) = parens $ sep [
        prettyT "when",
        pddlDoc p,
        pddlDoc e]

instance PDDLDocExpr Preference where
    pddlDocExpr (Preference n p) = parens $ sep [
        prettyT "preference",
        maybe emptyDoc pretty n,
        pddlDoc p]

instance PDDLDocExpr Start where
    pddlDocExpr Start = prettyT "start"

instance PDDLDocExpr End where
    pddlDocExpr End = prettyT "end"

instance {-# OVERLAPS #-} PDDLDoc t => PDDLDocExpr (At t) where
    pddlDocExpr (At t e) = parens $ sep [
        prettyT "at",
        pddlDoc t,
        pddlDoc e]

instance {-# OVERLAPS #-} PDDLDoc t => PDDLDocExpr (Over t) where
    pddlDocExpr (Over t e) = parens $ sep [
        prettyT "over",
        pddlDoc t,
        pddlDoc e]

instance PDDLDocExpr Always where
    pddlDocExpr (Always e) = parens $ sep [
        prettyT "always",
        pddlDoc e]

instance PDDLDocExpr Sometime where
    pddlDocExpr (Sometime e) = parens $ sep [
        prettyT "sometime",
        pddlDoc e]

instance PDDLDocExpr SometimeAfter where
    pddlDocExpr (SometimeAfter e1 e2) = parens $ sep [
        prettyT "sometime-after",
        pddlDoc e1,
        pddlDoc e2]

instance PDDLDocExpr SometimeBefore where
    pddlDocExpr (SometimeBefore e1 e2) = parens $ sep [
        prettyT "sometime-before",
        pddlDoc e1,
        pddlDoc e2]

instance PDDLDocExpr Within where
    pddlDocExpr (Within n e) = parens $ sep [
        prettyT "within",
        pretty n,
        pddlDoc e]

instance PDDLDocExpr AtMostOnce where
    pddlDocExpr (AtMostOnce e) = parens $ sep [
        prettyT "at-most-once",
        pddlDoc e]

instance PDDLDocExpr AlwaysWithin where
    pddlDocExpr (AlwaysWithin n e1 e2) = parens $ sep [
        prettyT "always-within",
        pretty n,
        pddlDoc e1,
        pddlDoc e2]

instance PDDLDocExpr HoldDuring where
    pddlDocExpr (HoldDuring n1 n2 e) = parens $ sep [
        prettyT "hold-during",
        pretty n1,
        pretty n2,
        pddlDoc e]

instance PDDLDocExpr HoldAfter where
    pddlDocExpr (HoldAfter n e) = parens $ sep [
        prettyT "hold-after",
        pretty n,
        pddlDoc e]

instance PDDLDocExpr OneOf where
    pddlDocExpr (OneOf el) = parens $ sep $
        prettyT "oneof":
        map pddlDoc el

instance PDDLDocExpr Unknown where
    pddlDocExpr (Unknown e) = parens $ sep [
        prettyT "unknown",
        pddlDoc e]


docNonEmpty :: PDDLDoc f => Text -> [f] -> Doc ann
docNonEmpty name ol =
    if (null ol) then emptyDoc else parens $ sep $
        pretty name : map pddlDoc ol

docList :: ([x] -> Doc ann) -> [x] -> Doc ann
docList _ [] = emptyDoc
docList f l = f l
docMaybe :: (x -> Doc ann) -> Maybe x -> Doc ann
docMaybe _ Nothing = emptyDoc
docMaybe f (Just x) = f x

docAnd :: forall a ann . (a -> Doc ann) -> [a] -> Doc ann
docAnd _ [] = parens emptyDoc
docAnd f [t] = f t
docAnd f tl = parens $ sep $ prettyT "and" : map f tl

------------------------------
-- Domain Description
------------------------------
data Domain a b g = Domain
    Name
    Requirements
    (Types TypedTypeExpr)
    (Constants TypedConstExpr)
    (Predicates TypedPredicateExpr)
    (Functions TypedFuncSkelExpr)
    (Constraints a)
    (Derived (TypedPredicateExpr, g))
    (Actions b)
    deriving (Data, Eq, Typeable)

instance (Data a, Data b, Data g) => HasName (Domain a b g)
instance (Data a, Data b, Data g) => HasRequirements (Domain a b g)
instance (Data a, Data b, Data g) => HasTypes TypedTypeExpr (Domain a b g)
instance (Data a, Data b, Data g) => HasConstants TypedConstExpr (Domain a b g)
instance (Data a, Data b, Data g) => HasPredicates TypedPredicateExpr (Domain a b g)
instance (Data a, Data b, Data g) => HasFunctions TypedFuncSkelExpr (Domain a b g)
instance (Data a, Data b, Data g) => HasConstraints a (Domain a b g)
instance (Data a, Data b, Data g) => HasDerived (TypedPredicateExpr, g) (Domain a b g)
instance (Data a, Data b, Data g) => HasActions b (Domain a b g)

instance (Data a, Data b, Data g, PDDLDoc a, PDDLDoc b, PDDLDoc g) =>
    PDDLDoc (Domain a b g) where
    pddlDoc domain = parens $ align $ vcat $
        prettyT "define" :
        parens (prettyT "domain" <+> pretty (getName domain)) :
         -- Requirement strings are prefixed with ':'
        (if (null $ getRequirements domain) then emptyDoc else parens
            (sep $
             map (pretty . (T.cons ':')) $
             "requirements" : getRequirements domain)) :
        docList (parens . sep . (prettyT ":types" :) . (:[]) . pddlDoc) (getTypes domain) :
        docList (parens . sep . (prettyT ":constants" :) . (:[]) . pddlDoc) (getConstants domain) :
        docList (parens . sep . (prettyT ":predicates" :) . map pddlDoc) (getPredicates domain) :
        docList (parens . sep . (prettyT ":functions" :) . (:[]) . pddlDoc) (getFunctions domain) :
        docList (parens . sep . (prettyT ":constraints" :) . (:[]) . docAnd pddlDoc) (getConstraints domain) :
        space :
        intersperse space (
          (flip map (getDerived domain) (\(p,b) ->
            parens $ sep $
              [ prettyT ":derived"
              , pddlDoc p
              , pddlDoc b ]))
          ++ (map pddlDoc $ getActions domain))


emptyDomain :: forall a b g. Domain a b g
emptyDomain = Domain
    (Name "empty")
    (Requirements [])
    (Types [])
    (Constants [])
    (Predicates [])
    (Functions [])
    (Constraints [])
    (Derived [])
    (Actions [])

------------------------------
-- Action Description
------------------------------
data Action p e = Action Name
    (Parameters TypedVarExpr)
    (Precondition p)
    (Effect e)
    deriving (Data,Typeable, Eq)
instance (Data p, Data e) => HasName (Action p e)
instance (Data p, Data e) => HasParameters TypedVarExpr (Action p e)
instance (Data p, Data e) => HasPrecondition p (Action p e)
instance (Data p, Data e) => HasEffect e (Action p e)
defaultAction :: (Data p, Data e) => Action p e
defaultAction = Action (Name "empty") (Parameters []) (Precondition []) (Effect [])

--instance (Data (Expr p), Data (Expr e), PDDLDocExpr p, PDDLDocExpr e) =>
--    PDDLDoc (Action (Expr p) (Expr e)) where
instance (Data p, Data t, Data ep, Data e, PDDLDoc p, PDDLDoc [t], PDDLDoc ep, PDDLDoc e)
    => PDDLDoc (Action (Maybe Text, p) ([t], Maybe ep, [e])) where
    pddlDoc a = parens $ sep [
        prettyT ":action" <+> (pretty $ getName a),
        prettyT ":parameters" <+> (parens $ pddlDoc $ getParameters a),
        docList ((prettyT ":precondition" <+>) . andDoc prefDoc) $ getPrecondition a,
        docList ((prettyT ":effect" <+>) . andDoc id . concatMap effectDoc) $ getEffect a]
        where
            andDoc :: forall a ann . (a -> Doc ann) -> [a] -> Doc ann
            andDoc f [t] = f t
            andDoc f tl = parens $ sep $
                prettyT "and"
                : map f tl
            prefDoc :: (Maybe Text, p) -> Doc ann
            prefDoc (Nothing, p) = pddlDoc p
            prefDoc (Just n, p) = parens $ sep [
                prettyT "preference",
                pretty n,
                pddlDoc p ]
            effectDoc :: ([t], Maybe ep, [e]) -> [Doc ann]
            effectDoc ([], ep, el) = condDoc ep el
            effectDoc (tl, ep, el) = [parens $ sep [
                prettyT "forall",
                parens (pddlDoc tl),
                andDoc id $ condDoc ep el]]
            condDoc :: Maybe ep -> [e] -> [Doc ann]
            condDoc Nothing el = map pddlDoc el
            condDoc (Just ep) el = [parens $ sep [
                prettyT "when",
                pddlDoc ep,
                andDoc pddlDoc el ]]



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

--instance
--    (Data (Expr a), Data (Expr b), Data (Expr c),
--     PDDLDocExpr a, PDDLDocExpr b, PDDLDocExpr c) =>
--    Show (Problem (Expr a) (Expr b) (Expr c)) where
instance (Data a, Data b, Data c,
        PDDLDoc a, PDDLDoc b, PDDLDoc c) =>
        PDDLDoc (Problem a b c) where
    pddlDoc prob = parens $ sep $
        prettyT "define" :
        (parens $ prettyT "problem" <+> (pretty $ getName prob)) :
        (parens $ prettyT ":domain" <+> (pretty $ getDomainName prob)) :
        (if null $ getRequirements prob then emptyDoc else
           (parens $ sep $ prettyT ":requirements" : map (pretty . (T.cons ':')) (getRequirements prob))) :
        --docNonEmpty ":objects" (getConstants prob) :
        docList (parens . (prettyT ":objects" <+>) . pddlDoc) (getConstants prob) :
        docList (parens . sep . (prettyT ":init" : ) . map pddlDoc) (getInitial prob) :
        docMaybe (parens . (prettyT ":goal" <+>) . pddlDoc) (getGoal prob) :
        docList (parens . (prettyT ":constraints" <+>) . docAnd pddlDoc) (getConstraints prob) :
        []



emptyProblem :: forall a b c. Problem a b c
emptyProblem = Problem
    (Name "empty")
    (DomainName "empty")
    (Requirements [])
    (Constants [])
    (Initial [])
    (Goal Nothing)
    (Constraints [])
