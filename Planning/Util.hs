{-# OPTIONS 
 -fglasgow-exts
 -fallow-overlapping-instances
 -fallow-undecidable-instances #-}
module Planning.Util where

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




