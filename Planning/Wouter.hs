{-# OPTIONS -XFlexibleInstances #-}
{-# OPTIONS -XMultiParamTypeClasses #-}
{-# OPTIONS -XTypeOperators #-}
module Planning.Wouter where

-- Thank you, Wouter Swierstra

infixr 6 :+:
data (f :+: g) e = Inl (f e) | Inr (g e) deriving Eq

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl e1) = Inl (fmap f e1)
    fmap f (Inr e2) = Inr (fmap f e2)

class (Functor sub, Functor sup) => sub :<: sup where
    inj :: sub a -> sup a

instance Functor f => (:<:) f f where
    inj = id

instance (Functor f, Functor g) => (:<:) f (f :+: g) where
    inj = Inl

instance (Functor f, Functor g, Functor h, (:<:) f g) => (:<:) f (h :+: g) where
    inj = Inr . inj

data Expr f = In (f (Expr f))
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

