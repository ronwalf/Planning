{-# LANGUAGE 
    DeriveDataTypeable,
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses,
    OverlappingInstances,
    RankNTypes,
    ScopedTypeVariables,
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
    docList, docMaybe, docNonEmpty
) where

import Data.Data
import Data.Function (on)
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

instance PDDLDoc String where
    pddlDoc = text

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

instance PDDLDoc t => PDDLDoc [Expr (Typed t)] where
    pddlDoc tl = 
        let
            groups :: [[Expr (Typed t)]]
            groups = groupBy ((==) `on` getType) tl
        in
        sep $ fst $ foldr typelistDoc ([], []) groups
        where
        
        typelistDoc :: [Expr (Typed t)] -> ([Doc], [String]) -> ([Doc], [String])
        typelistDoc [] x = x
        typelistDoc tg@(h:_) (doc, ptype)
            | getType h == ptype 
                = (sep (docSameTypes tg) : doc, ptype)
            | otherwise
                = let dt = char '-' <+> docType (getType h) in
                    (sep (docSameTypes tg ++ [dt]) : doc, getType h)
        docSameTypes :: [Expr (Typed t)] -> [Doc]
        docSameTypes = map (pddlDoc . removeType)
        docType :: [String] -> Doc
        docType [] = text "object"
        docType [t] = text t
        docType l = parens $ sep $ text "either" : map text l

-- No PDDLDocExpr for Typed, which gives us a way to distinquish
-- typed instances and render them differently.
instance PDDLDocExpr t => PDDLDocExpr (Atomic (Expr t)) where
    pddlDocExpr (Atomic p tl) = parens $ hsep $
        (text p) : map pddlDoc tl

instance PDDLDoc t => PDDLDocExpr (Atomic (Expr (Typed t))) where
    pddlDocExpr (Atomic p tl) = parens $ 
        (text p) <+> pddlDoc tl

instance PDDLDocExpr And where
    pddlDocExpr (And el) = parens $ sep $ text "and" : [pddlDocExpr e | In e <- el]

instance PDDLDocExpr (Exists TypedVarExpr) where
    pddlDocExpr (Exists vl (In e)) = parens $ sep [
        text "exists",
        parens (pddlDoc vl),
        pddlDocExpr e ]

instance PDDLDocExpr (ForAll TypedVarExpr) where
    pddlDocExpr (ForAll vl (In e)) = parens $ sep [
        text "forall",
        parens (pddlDoc vl),
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


instance PDDLDoc p => PDDLDocExpr (When p) where
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

instance PDDLDoc t => PDDLDocExpr (At t) where
    pddlDocExpr (At t e) = parens $ sep [
        text "at",
        pddlDoc t,
        pddlDoc e]

instance PDDLDoc t => PDDLDocExpr (Over t) where
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


docNonEmpty :: PDDLDoc f => String -> [f] -> Doc
docNonEmpty name ol =
    if (null ol) then empty else parens $ sep $
        text name : map pddlDoc ol

docList :: ([x] -> Doc) -> [x] -> Doc
docList _ [] = empty
docList f l = f l
docMaybe :: (x -> Doc) -> Maybe x -> Doc
docMaybe _ Nothing = empty
docMaybe f (Just x) = f x


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
    pddlDoc domain = parens $ ($$) (text "define") $ vcat $
        parens (text "domain" <+> text (getName domain)) :
         -- Requirement strings are prefixed with ':'
        (if (null $ getRequirements domain) then empty else parens
            (sep $ 
             map (text . (':':)) $ 
             "requirements" : getRequirements domain)) :
        docList (parens . sep . (text ":types" :) . (:[]) . pddlDoc) (getTypes domain) :
        docList (parens . sep . (text ":constants" :) . (:[]) . pddlDoc) (getConstants domain) :
        docList (parens . sep . (text ":predicates" :) . map pddlDoc) (getPredicates domain) :
        docList (parens . sep . (text ":functions" :) . (:[]) . pddlDoc) (getFunctions domain) :
        maybe empty (\constr -> parens $ sep [text ":constraints", pddlDoc constr])
            (getConstraints domain) :
        space :
        intersperse space (
          (flip map (getDerived domain) (\(p,b) ->
            parens $ sep $ 
              [ text ":derived"
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
    (Constraints Nothing)
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
    => PDDLDoc (Action (Maybe String, p) ([t], Maybe ep, [e])) where
    pddlDoc a = parens $ sep [
        text ":action" <+> (text $ getName a),
        --text ":parameters" <+> (parens $ hsep $ map pddlDoc $ getParameters a), 
        text ":parameters" <+> (parens $ pddlDoc $ getParameters a),
        docList ((text ":precondition" <+>) . andDoc prefDoc) $ getPrecondition a,
        docList ((text ":effect" <+>) . andDoc id . concatMap effectDoc) $ getEffect a]
        where
            andDoc :: forall a . (a -> Doc) -> [a] -> Doc
            andDoc f [t] = f t
            andDoc f tl = parens $ sep $
                text "and"
                : map f tl
            prefDoc :: (Maybe String, p) -> Doc
            prefDoc (Nothing, p) = pddlDoc p
            prefDoc (Just n, p) = parens $ sep [
                text "preference",
                text n,
                pddlDoc p ]
            effectDoc :: ([t], Maybe ep, [e]) -> [Doc]
            effectDoc ([], ep, el) = condDoc ep el
            effectDoc (tl, ep, el) = [parens $ sep [
                text "forall",
                parens (pddlDoc tl),
                andDoc id $ condDoc ep el]]
            condDoc :: Maybe ep -> [e] -> [Doc]
            condDoc Nothing el = map pddlDoc el
            condDoc (Just ep) el = [parens $ sep [
                text "when",
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
        text "define" :
        (parens $ text "problem" <+> (text $ getName prob)) :
        (parens $ text ":domain" <+> (text $ getDomainName prob)) :
        (if null $ getRequirements prob then empty else 
           (parens $ sep $ text ":requirements" : map (text . (':':)) (getRequirements prob))) :
        --docNonEmpty ":objects" (getConstants prob) :
        docList (parens . (text ":objects" <+>) . pddlDoc) (getConstants prob) :
        docList (parens . sep . (text ":init" : ) . map pddlDoc) (getInitial prob) : 
        docMaybe (parens . (text ":goal" <+>) . pddlDoc) (getGoal prob) :
        docMaybe (parens . (text ":constraints" <+>) . pddlDoc) (getConstraints prob) :
        []

       
    
emptyProblem :: forall a b c. Problem a b c
emptyProblem = Problem
    (Name "empty")
    (DomainName "empty")
    (Requirements [])
    (Constants [])
    (Initial [])
    (Goal Nothing)
    (Constraints Nothing)
