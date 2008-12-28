{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}
module Planning.Util where

import Data.List

import Planning.Expressions

-- nnf helper class
-- nnf' takes a bool arg, False for negated, True for not-negated.
class (Functor f, Functor g) => NNF f g where
    nnf' :: Bool -> f (Expr g) -> Expr g
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
conjuncts (In x) = conjuncts' x
conjunct el = eAnd $ concatMap conjuncts el

instance (Conjuncts f h, Conjuncts g h) => Conjuncts (f :+: g) h where
    conjuncts' (Inl x) = conjuncts' x
    conjuncts' (Inr y) = conjuncts' y

instance (:<:) (Atomic t) g => Conjuncts (Atomic t) g where
    conjuncts' (Atomic p tl) = [eAtomic p tl]
instance (:<:) And g => Conjuncts And g where 
    conjuncts' (And el) = el
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

