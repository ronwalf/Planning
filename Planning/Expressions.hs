module Planning.Expressions (
    module Planning.Wouter,
    Const(..), eConst,
    Var(..), eVar,

    Atomic(..), eAtomic,
    And(..), eAnd,
    Imply(..), eImply,
    Not(..), eNot,
    Or(..), eOr,

    Exists(..), eExists,
    ForAll(..), eForAll


) where

import Planning.Wouter


data Const e = Const String deriving Eq
instance Functor Const where
    fmap f (Const x) = Const x
instance FuncEq Const where
    funcEq (Const x) (Const y) = x == y
eConst x = inject (Const x)

data Var e = Var String deriving Eq
instance Functor Var where
    fmap f (Var x) = Var x
instance FuncEq Var where
    funcEq (Var x) (Var y) = x == y
eVar x = inject (Var x)

data Atomic t e = Atomic String [t] deriving Eq
instance Functor (Atomic a) where
    fmap f (Atomic p tl) = Atomic p tl
instance (Eq t) => FuncEq (Atomic t) where
    funcEq (Atomic p1 tl1) (Atomic p2 tl2) = (p1 == p2) && (tl1 == tl2)
eAtomic p tl = inject (Atomic p tl)

data And e = And [e] deriving Eq
instance Functor And where
    fmap f (And el) = And $ map f el
instance FuncEq And where
    funcEq (And el1) (And el2) = el1 == el2
eAnd [e] = e
eAnd el = inject (And el)

data Or e = Or [e] deriving Eq
instance Functor Or where
    fmap f (Or el) = Or $ map f el
instance FuncEq Or where
    funcEq (Or el1) (Or el2) = el1 == el2
eOr [e] = e
eOr el = inject (Or el)


data Imply e = Imply e e deriving Eq
instance Functor Imply where
    fmap f (Imply e1 e2) = Imply (f e1) (f e2)
instance FuncEq Imply where
    funcEq (Imply x1 y1) (Imply x2 y2) = (x1 == x2) && (y1 == y2)
eImply e1 e2 = inject (Imply e1 e2)

data Not e = Not e deriving Eq
instance Functor Not where
    fmap f (Not e) = Not $ f e
instance FuncEq Not where
    funcEq (Not x) (Not y) = x == y
eNot e = inject (Not e)

data ForAll v e = ForAll [v] e deriving Eq
instance Functor (ForAll vl) where
    fmap f (ForAll vl e) = ForAll vl $ f e
instance Eq v => FuncEq (ForAll v) where
    funcEq (ForAll vl1 e1) (ForAll vl2 e2) = (vl1 == vl2) && (e1 == e2)
eForAll [] e = e
eForAll vl e = inject (ForAll vl e)


data Exists v e = Exists [v] e deriving Eq
instance Functor (Exists vl) where
    fmap f (Exists vl e) = Exists vl $ f e
instance Eq v => FuncEq (Exists v) where
    funcEq (Exists vl1 e1) (Exists vl2 e2) = (vl1 == vl2) && (e1 == e2)
eExists [] e = e
eExists vl e = inject (Exists vl e)

