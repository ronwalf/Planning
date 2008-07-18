{-# OPTIONS 
 -fglasgow-exts
 -fallow-overlapping-instances
  #-}
module Planning.Wouter where

import Data.Generics hiding ((:+:), Inl, Inr)
-- Thank you, Wouter Swierstra

{-
--------------------------------
-- Records 
--------------------------------
infixr 6 :@:
data (f :@: g) e = Recb (f e) (g e) deriving Eq

instance (Functor f, Functor g) => Functor (f :@: g) where
    fmap f (Recb r1 r2) = Recb (fmap f r1) (fmap f r2)

class (Functor sub, Functor sup) => sub :<@: sup where
    rGet :: sup a -> sub a
    rSet :: sub a -> sup a -> sup a

instance Functor f => (:<@:) f f where
    rGet = id
    rSet x _ = x

instance (Functor f, Functor g) => (:<@:) f (f :@: g) where
    rGet (Recb x y) = x
    rSet x (Recb _ y) = Recb x y

instance (Functor f, Functor g, Functor h, (:<@:) f g) =>
    (:<@:) f (h :@: g) where
    rGet (Recb _ y) = rGet y
    rSet x (Recb y z) = Recb y (rSet x z)


data Record f = Rec (f (Record f))

class Functor f => RecordField f where
    dVal :: f a

instance (RecordField f, RecordField g) =>
    RecordField (f :@: g) where
    dVal = Recb dVal dVal

dRec :: RecordField f => Record f
dRec = Rec dVal

recGet (Rec r) = rGet r
recSet v (Rec r) = Rec $ rSet v r
-}

--------------------------------
-- Expressions 
--------------------------------

infixr 6 :+:
data (f :+: g) e = Inl (f e) | Inr (g e) deriving (Data, Eq)

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl e1) = Inl (fmap f e1)
    fmap f (Inr e2) = Inr (fmap f e2)

instance (Typeable1 f, Typeable1 g) => Typeable1 (f :+: g) where
    typeOf1 l = mkTyConApp (mkTyCon "Planning.Wouter.:+:") [typeOf1 x, typeOf1 y] where
        Inl x = (Inl undefined) `asTypeOf` l
        Inr y = (Inr undefined) `asTypeOf` l


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

