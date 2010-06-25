{-# LANGUAGE
    FlexibleInstances,
    MultiParamTypeClasses,
    OverlappingInstances,
    StandaloneDeriving,
    TypeOperators,
    UndecidableInstances 
  #-}
module Planning.Wouter (
 (:+:)(..),
 (:<:)(..),
 Expr(..),
 inject,
 foldExpr,
 FuncEq(..),
 FuncOrd(..)
) where

import Data.Data

-- Thank you, Wouter Swierstra

--------------------------------
-- Expressions 
--------------------------------

infixr 6 :+:
data (f :+: g) e = Inl (f e) | Inr (g e) deriving (Eq)

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl e1) = Inl (fmap f e1)
    fmap f (Inr e2) = Inr (fmap f e2)

instance (Typeable1 f, Typeable1 g) => Typeable1 (f :+: g) where
    typeOf1 l = mkTyConApp (mkTyCon "Planning.Wouter.:+:") [typeOf1 x, typeOf1 y] where
        Inl x = (Inl undefined) `asTypeOf` l
        Inr y = (Inr undefined) `asTypeOf` l

deriving instance (
   Typeable1 f,
    Typeable1 g,
    Typeable e,
    Data (f e),
    Data (g e))
        => Data ((f :+: g) e)
 

class (Functor sub, Functor sup) => sub :<: sup where
    inj :: sub a -> sup a

instance Functor f => (:<:) f f where
    inj = id

instance (Functor f, Functor g) => (:<:) f (f :+: g) where
    inj = Inl

instance (Functor f, Functor g, Functor h, (:<:) f g) => (:<:) f (h :+: g) where
    inj = Inr . inj

newtype Expr f = In (f (Expr f))
instance Typeable1 f => Typeable (Expr f) where
    typeOf e = mkTyConApp (mkTyCon "Planning.Wouter.Expr") [typeOf1 x]
        where In x = (In undefined) `asTypeOf` e
deriving instance (Typeable1 a, Data (a (Expr a))) => Data (Expr a)

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

--------------------------------
-- Utilities
--------------------------------

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

class Functor f => FuncEq f where
    funcEq :: FuncEq g => f (Expr g) -> f (Expr g) -> Bool

instance (FuncEq f, FuncEq g) => FuncEq (f :+: g) where
    funcEq (Inl x) (Inl y) = funcEq x y
    funcEq (Inr x) (Inr y) = funcEq x y
    funcEq _ _ = False

instance (FuncEq f) => Eq (Expr f) where
    (In x) == (In y) = funcEq x y


class (Functor f, FuncEq f) => FuncOrd f where
    funcCompare:: FuncOrd g => f (Expr g) -> f (Expr g) -> Ordering

instance (FuncOrd f, FuncOrd g) => FuncOrd (f :+: g) where
    funcCompare (Inl x) (Inl y) = funcCompare x y
    funcCompare (Inr x) (Inr y) = funcCompare x y
    funcCompare (Inl _) (Inr _) = GT
    funcCompare (Inr _) (Inl _) = LT

instance (FuncOrd f) => Ord (Expr f) where
    compare (In x) (In y) = funcCompare x y

