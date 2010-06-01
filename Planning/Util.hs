{-# LANGUAGE
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses,
    OverlappingInstances,
    ScopedTypeVariables,
    TypeOperators,
    UndecidableInstances 
  #-}
module Planning.Util where

import Data.List

import Planning.Expressions

-- nnf helper class
-- nnf' takes a bool arg, False for negated, True for not-negated.
class (Functor f, Functor g) => NNF f g where
    nnf' :: Bool -> f (Expr g) -> Expr g
nnf :: (NNF f f) => Expr f -> Expr f
nnf (In e) = nnf' True e

instance (NNF f h, NNF g h) => NNF (f :+: g) h where
    nnf' b (Inr x) = nnf' b x
    nnf' b (Inl y) = nnf' b y

instance ((:<:) Not g, (:<:) (Atomic t) g) => NNF (Atomic t) g where
    nnf' True (Atomic p tl) = eAtomic p tl
    nnf' False (Atomic p tl) = eNot $ eAtomic p tl

instance (NNF g g) => NNF Not g where
    nnf' b (Not (In e)) = nnf' (not b) e

instance ((:<:) And g, (:<:) Or g, NNF g g) => NNF And g where
    nnf' True (And el) = eAnd [nnf' True e | In e <-  el]
    nnf' False (And el) = eOr [nnf' False e | In e <- el]

instance ((:<:) And g, (:<:) Or g, NNF g g) => NNF Or g where
    nnf' True (Or el) = eOr [nnf' True e | In e <-  el]
    nnf' False (Or el) = eAnd [nnf' True e | In e <-  el]

instance ((:<:) (Exists t) g, (:<:) (ForAll t) g, NNF g g) => NNF (Exists t) g where
    nnf' True (Exists vl (In e)) = eExists vl $ nnf' True e
    nnf' False (Exists vl (In e)) = eForAll vl $ nnf' False e

instance ((:<:) (ForAll t) g, (:<:) (Exists t) g, NNF g g) => NNF (ForAll t) g where
    nnf' True (ForAll vl (In e)) = eForAll vl $ nnf' True e
    nnf' False (ForAll vl (In e)) = eExists vl $ nnf' False e

instance ((:<:) And g, (:<:) Imply g, NNF g g) => NNF Imply g where
    nnf' True (Imply (In e1) (In e2)) = eImply (nnf' True e1) (nnf' True e2)
    nnf' False (Imply (In e1) (In e2)) = eAnd [nnf' True e1, nnf' False e2]

instance ((:<:) Preference g, NNF g g) => NNF Preference g where
    nnf' b (Preference n (In e)) = ePreference n $ nnf' b e



class (Functor f, Functor g) => Conjuncts f g where
    conjuncts' :: f (Expr g) -> [Expr g]
conjuncts :: (Conjuncts f f) => Expr f -> [Expr f]
conjuncts (In x) = conjuncts' x
conjunct :: (And :<: f, Conjuncts f f) => [Expr f] -> Expr f
conjunct el = eAnd $ concatMap conjuncts el

instance (Conjuncts f h, Conjuncts g h) => Conjuncts (f :+: g) h where
    conjuncts' (Inl x) = conjuncts' x
    conjuncts' (Inr y) = conjuncts' y

instance (:<:) (Atomic t) g => Conjuncts (Atomic t) g where
    conjuncts' (Atomic p tl) = [eAtomic p tl]
instance ((:<:) And g, Conjuncts g g) => Conjuncts And g where 
    conjuncts' (And el) = concatMap conjuncts el
instance (:<:) Or g => Conjuncts Or g where
    conjuncts' (Or el) = [eOr el]
instance (:<:) Not g => Conjuncts Not g where
    conjuncts' (Not e) = [eNot e]
instance (:<:) (ForAll t) g => Conjuncts (ForAll t) g where
    conjuncts' (ForAll vl e) = [eForAll vl e]
instance (:<:) (Exists t) g => Conjuncts (Exists t) g where
    conjuncts' (Exists vl e) = [eExists vl e]
instance (:<:) Imply g => Conjuncts Imply g where
    conjuncts' (Imply e1 e2) = [eImply e1 e2]
instance (:<:) Preference g => Conjuncts Preference g where
    conjuncts' (Preference n e) = [ePreference n e]
instance (:<:) (When p) g => Conjuncts (When p) g where
    conjuncts' (When p e) = [eWhen p e]
instance (:<:) OneOf g => Conjuncts OneOf g where
    conjuncts' (OneOf el) = [eOneOf el]

class (Functor f) => FreeVarsFindable f where
    findFreeVars' :: f [Expr Var] -> [Expr Var]
findFreeVars :: FreeVarsFindable f => Expr f -> [Expr Var]
findFreeVars = foldExpr findFreeVars'

instance (FreeVarsFindable f, FreeVarsFindable g) => FreeVarsFindable (f :+: g) where
    findFreeVars' (Inl x) = findFreeVars' x
    findFreeVars' (Inr y) = findFreeVars' y

instance FreeVarsFindable Var where
    findFreeVars' (Var n) = [eVar n]
instance FreeVarsFindable Const where
    findFreeVars' _ = []
instance FreeVarsFindable Function where
    findFreeVars' (Function _ tl) = concat tl
instance FreeVarsFindable t => FreeVarsFindable (Typed (Expr t)) where
    findFreeVars' (Typed t _) = findFreeVars t

instance FreeVarsFindable t => FreeVarsFindable (Atomic (Expr t)) where
    findFreeVars' (Atomic _ tl) = concatMap findFreeVars tl
instance FreeVarsFindable And where
    findFreeVars' (And el) = concat el
instance FreeVarsFindable Or where
    findFreeVars' (Or el) = concat el
instance FreeVarsFindable Not where
    findFreeVars' (Not e) = e
instance FreeVarsFindable t => FreeVarsFindable (ForAll (Expr t)) where
    findFreeVars' (ForAll vl e) = e \\ concatMap findFreeVars vl
instance FreeVarsFindable t => FreeVarsFindable (Exists (Expr t)) where
    findFreeVars' (Exists vl e) = e \\ concatMap findFreeVars vl
instance FreeVarsFindable Imply where
    findFreeVars' (Imply e1 e2) = e1 ++ e2
instance FreeVarsFindable Preference where
    findFreeVars' (Preference _ e) = e
instance FreeVarsFindable f => FreeVarsFindable (When (Expr f)) where
    findFreeVars' (When p e) = findFreeVars p ++ e



-- Find all atoms in an expression
class (Functor f) => AtomsFindable f g where
    findAtoms' :: f [Expr (Atomic g)] -> [Expr (Atomic g)]
findAtoms :: AtomsFindable f g => Expr f -> [Expr (Atomic g)]
findAtoms = foldExpr findAtoms'

instance (AtomsFindable f g, AtomsFindable h g) => AtomsFindable (f :+: h) g where
    findAtoms' (Inl x) = findAtoms' x
    findAtoms' (Inr y) = findAtoms' y
instance AtomsFindable (Atomic g) g where
    findAtoms' (Atomic p tl) = [eAtomic p tl]
instance AtomsFindable And g where
    findAtoms' (And el) = concat el
instance AtomsFindable Or g where
    findAtoms' (Or el) = concat el
instance AtomsFindable Not g where
    findAtoms' (Not e) = e
instance AtomsFindable (ForAll t) g where
    findAtoms' (ForAll _ e) = e
instance AtomsFindable (Exists t) g where
    findAtoms' (Exists _ e) = e 
instance AtomsFindable Imply g where
    findAtoms' (Imply e1 e2) = e1 ++ e2
instance AtomsFindable Preference g where
    findAtoms' (Preference _ e) = e
instance AtomsFindable f g => AtomsFindable (When (Expr f)) g where
    findAtoms' (When p e) = findAtoms p ++ e
instance AtomsFindable OneOf g where
    findAtoms' (OneOf el) = concat el


-- Break apart OneOf expressions into a list of possibilities
class (Functor f) => OneOfFindable f g where
    findOneOf' :: f [Expr g] -> [Expr g]
findOneOf :: OneOfFindable f g => Expr f -> [Expr g]
findOneOf = foldExpr findOneOf'

instance (OneOfFindable f g, OneOfFindable h g) => OneOfFindable (f :+: h) g where
    findOneOf' (Inl x) = findOneOf' x
    findOneOf' (Inr y) = findOneOf' y
instance (Atomic t :<: g) => OneOfFindable (Atomic t) g where
    findOneOf' (Atomic p tl) = [eAtomic p tl]
instance (And :<: g, Conjuncts g g) => OneOfFindable And g where
    findOneOf' (And el) = map (eAnd . conjuncts) $ cdots el
        where
            cdots :: [[Expr g]] -> [Expr g]
            cdots [] = []
            cdots [h] = h
            cdots (h : n : t) =
                cdots ([ eAnd [p1, p2] | p1 <- h, p2 <- n ] : t)
instance (Not :<: g) => OneOfFindable Not g where
    findOneOf' (Not e) = map eNot e -- Technically wrong, but what the heck would not oneOf mean, anyway?
instance (ForAll t :<: g) => OneOfFindable (ForAll t) g where
    findOneOf' (ForAll vl e) = map (eForAll vl) e
instance (Exists t :<: g) => OneOfFindable (Exists t) g where
    findOneOf' (Exists vl e) = map (eExists vl) e
instance (When f :<: g) => OneOfFindable (When f) g where
    findOneOf' (When p e) = map (eWhen p) e
instance OneOfFindable OneOf g where
    findOneOf' (OneOf el) = concat el


