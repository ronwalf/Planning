{-# OPTIONS
 -fglasgow-exts
 -fallow-undecidable-instances
 #-}
module Planning.Expressions (
    module Planning.Wouter,

    -- Terms
    Const(..), eConst,
    Var(..), eVar,
    Function(..), eFunc,

    -- Typing
    Typed(..), eTyped,
    TypedConst,
    TypedConstExpr,
    TypedVar,
    TypedVarExpr,
    TypedFunc,
    TypedFuncExpr,
    Untypeable, removeType,

    -- First Order Logic
    Atomic(..), eAtomic,
    And(..), eAnd,
    Imply(..), eImply,
    Not(..), eNot,
    Or(..), eOr,

    Exists(..), eExists,
    ForAll(..), eForAll,

    When(..), eWhen,

    -- Preferences
    Preference(..), ePreference,
    unPreference,

    -- Timing
    Start(..), eStart,
    End(..), eEnd,
    All(..), eAll,
    At(..), eAt,
    Over(..), eOver,
    Always(..), eAlways,
    Sometime(..), eSometime,
    Within(..), eWithin,
    AtMostOnce(..), eAtMostOnce,
    SometimeAfter(..), eSometimeAfter,
    SometimeBefore(..), eSometimeBefore,
    AlwaysWithin(..), eAlwaysWithin,
    HoldDuring(..), eHoldDuring,
    HoldAfter(..), eHoldAfter
   

) where

--import Data.Generics hiding ((:+:), Inl, Inr)
import Data.Generics (Data, Typeable, Typeable1, Typeable2)

import Planning.Wouter

---------------------------------
-- Term Holders
---------------------------------
data Const e = Const String deriving (Data, Eq)
deriving instance Typeable1 Const
instance Functor Const where
    fmap f (Const x) = Const x
instance FuncEq Const where
    funcEq (Const x) (Const y) = x == y
eConst x = inject (Const x)

data Var e = Var String deriving (Data, Eq)
deriving instance Typeable1 Var
instance Functor Var where
    fmap f (Var x) = Var x
instance FuncEq Var where
    funcEq (Var x) (Var y) = x == y
eVar x = inject (Var x)

data Function e = Function String [e] deriving (Data, Eq)
deriving instance Typeable1 Function
instance Functor Function where
    fmap f (Function n tl) = Function n $ map f tl
instance FuncEq Function where
    funcEq (Function n1 tl1) (Function n2 tl2) = (n1 == n2) && (tl1 == tl2)
eFunc n tl = inject (Function n tl)

---------------------------------------
-- Typing (commonly used to type Terms)
---------------------------------------

data Typed t e = Typed t (Expr Const) deriving (Data, Eq)
deriving instance Typeable2 Typed
instance Functor (Typed t) where
    fmap f (Typed e t) = Typed e t
instance Eq t => FuncEq (Typed t) where
    funcEq (Typed e1 t1) (Typed e2 t2) = (e1 == e2) && (t1 == t2)
eTyped e t = inject (Typed e t)
type TypedConst = Typed (Expr Const)
type TypedConstExpr = Expr (TypedConst :+: Const)
type TypedVar = Typed (Expr Var)
type TypedVarExpr = Expr (TypedVar :+: Var)
type TypedFunc = Typed (Expr Function)
type TypedFuncExpr = Expr (TypedFunc :+: Function :+: Var :+: TypedVar)

class (Functor f, Functor g) => Untypeable g f where
    untype :: f (Expr g) -> Expr g
instance (Functor h, Untypeable h f, Untypeable h g) => Untypeable h (f :+: g) where
    untype (Inl x) = untype x
    untype (Inr y) = untype y
instance (:<:) Const g => Untypeable g (Typed (Expr Const)) where
    untype (Typed (In (Const c)) _) = eConst c
instance (:<:) Var g => Untypeable g (Typed (Expr Var)) where
    untype (Typed (In (Var v)) _ ) = eVar v
instance (:<:) Const g => Untypeable g Const where
    untype (Const c) = eConst c
instance (:<:) Var g => Untypeable g Var where
    untype (Var v) = eVar v

removeType :: Untypeable g f => Expr f -> Expr g
removeType = foldExpr untype



--------------------------------------------------------
-- Literals
--------------------------------------------------------

data Atomic t e = Atomic String [t] deriving (Data, Eq)
deriving instance Typeable2 Atomic
instance Functor (Atomic a) where
    fmap f (Atomic p tl) = Atomic p tl
instance (Eq t) => FuncEq (Atomic t) where
    funcEq (Atomic p1 tl1) (Atomic p2 tl2) = (p1 == p2) && (tl1 == tl2)
eAtomic p tl = inject (Atomic p tl)

data Not e = Not e deriving (Data, Eq)
deriving instance Typeable1 Not
instance Functor Not where
    fmap f (Not e) = Not $ f e
instance FuncEq Not where
    funcEq (Not x) (Not y) = x == y
eNot e = inject (Not e)

---------------------------------------
-- First Order Logic Connectives 
---------------------------------------
data And e = And [e] deriving (Data, Eq)
deriving instance Typeable1 And
instance Functor And where
    fmap f (And el) = And $ map f el
instance FuncEq And where
    funcEq (And el1) (And el2) = el1 == el2
eAnd [e] = e
eAnd el = inject (And el)

data Or e = Or [e] deriving (Data, Eq)
deriving instance Typeable1 Or
instance Functor Or where
    fmap f (Or el) = Or $ map f el
instance FuncEq Or where
    funcEq (Or el1) (Or el2) = el1 == el2
eOr [e] = e
eOr el = inject (Or el)

data Imply e = Imply e e deriving (Data, Eq)
deriving instance Typeable1 Imply
instance Functor Imply where
    fmap f (Imply e1 e2) = Imply (f e1) (f e2)
instance FuncEq Imply where
    funcEq (Imply x1 y1) (Imply x2 y2) = (x1 == x2) && (y1 == y2)
eImply e1 e2 = inject (Imply e1 e2)

data ForAll v e = ForAll [v] e deriving (Data, Eq)
deriving instance Typeable2 ForAll
instance Functor (ForAll vl) where
    fmap f (ForAll vl e) = ForAll vl $ f e
instance Eq v => FuncEq (ForAll v) where
    funcEq (ForAll vl1 e1) (ForAll vl2 e2) = (vl1 == vl2) && (e1 == e2)
eForAll [] e = e
eForAll vl e = inject (ForAll vl e)

data Exists v e = Exists [v] e deriving (Data, Eq)
deriving instance Typeable2 Exists
instance Functor (Exists vl) where
    fmap f (Exists vl e) = Exists vl $ f e
instance Eq v => FuncEq (Exists v) where
    funcEq (Exists vl1 e1) (Exists vl2 e2) = (vl1 == vl2) && (e1 == e2)
eExists [] e = e
eExists vl e = inject (Exists vl e)


data When p e = When p e deriving (Data, Eq)
deriving instance Typeable2 When
instance Functor (When p) where
    fmap f (When p e) = When p $ f e
instance Eq p => FuncEq (When p) where
    funcEq (When p1 e1) (When p2 e2) = (p1 == p2) && (e1 == e2)
eWhen p e = inject (When p e)

----------------------------------
-- Preferences
----------------------------------
data Preference e = Preference (Maybe String) e deriving (Data, Eq)
deriving instance Typeable1 Preference
instance Functor Preference where
    fmap f (Preference n e) = Preference n $ f e
instance FuncEq Preference where
    funcEq (Preference n1 e1) (Preference n2 e2) = (n1 == n2) && (e1 == e2)
ePreference n e = inject (Preference n e)

class Functor f => UnPreference g f where
    unPreference :: f (Maybe g) -> Maybe g


----------------------------------
-- Timing
----------------------------------
data Start e = Start deriving (Data, Eq)
deriving instance Typeable1 Start
instance Functor Start where
    fmap f Start = Start
instance FuncEq Start where
    funcEq _ _ = True
eStart :: (:<:) Start f => Expr f
eStart = inject Start

data End e = End deriving (Data, Eq)
deriving instance Typeable1 End
instance Functor End where
    fmap f End = End
instance FuncEq End where
    funcEq _ _ = True
eEnd :: (:<:) End f => Expr f
eEnd = inject End

data All e = All deriving (Data, Eq)
deriving instance Typeable1 All
instance Functor All where
    fmap f All = All
instance FuncEq All where
    funcEq _ _ = True
eAll :: (:<:) All f => Expr f
eAll = inject All


data At t e = At t e deriving (Data, Eq)
deriving instance Typeable2 At
instance Functor (At t) where
    fmap f (At t e) = At t $ f e
instance Eq t => FuncEq (At t) where
    funcEq (At t1 e1) (At t2 e2) = (t1 == t2) && (e1 == e2)
eAt t e = inject (At t e)

data Over t e = Over t e deriving (Data, Eq)
deriving instance Typeable2 Over
instance Functor (Over t) where
    fmap f (Over t e) = Over t $ f e
instance Eq t => FuncEq (Over t) where
    funcEq (Over t1 e1) (Over t2 e2) = (t1 == t2) && (e1 == e2)
eOver t e = inject (Over t e)

data Always e = Always e deriving (Data, Eq)
deriving instance Typeable1 Always
instance Functor Always where
    fmap f (Always e) = Always $ f e
instance FuncEq Always where
    funcEq (Always e1) (Always e2) = e1 == e2
eAlways e = inject (Always e)

data Sometime e = Sometime e deriving (Data, Eq)
deriving instance Typeable1 Sometime
instance Functor Sometime where
    fmap f (Sometime e) = Sometime $ f e
instance FuncEq Sometime where
    funcEq (Sometime e1) (Sometime e2) = e1 == e2
eSometime e = inject (Sometime e)

data Within e = Within Double e deriving (Data, Eq)
deriving instance Typeable1 Within
instance Functor Within where
    fmap f (Within n e) = Within n $ f e
instance FuncEq Within where
    funcEq (Within n1 e1) (Within n2 e2) = (n1 == n2) && (e1 == e2)
eWithin d e = inject (Within d e)

data AtMostOnce e = AtMostOnce e deriving (Data, Eq)
deriving instance Typeable1 AtMostOnce
instance Functor AtMostOnce where
    fmap f (AtMostOnce e) = AtMostOnce $ f e
instance FuncEq AtMostOnce where
    funcEq (AtMostOnce e1) (AtMostOnce e2) = e1 == e2
eAtMostOnce e = inject (AtMostOnce e)

data SometimeAfter e = SometimeAfter e e deriving (Data, Eq)
deriving instance Typeable1 SometimeAfter
instance Functor SometimeAfter where
    fmap f (SometimeAfter e1 e2) = SometimeAfter (f e1) (f e2)
instance FuncEq SometimeAfter where
    funcEq (SometimeAfter e11 e12 ) (SometimeAfter e21 e22) = (e11 == e21) && (e12 == e22)
eSometimeAfter e1 e2 = inject (SometimeAfter e1 e2)

data SometimeBefore e = SometimeBefore e e deriving (Data, Eq)
deriving instance Typeable1 SometimeBefore
instance Functor SometimeBefore where
    fmap f (SometimeBefore e1 e2) = SometimeBefore (f e1) (f e2)
instance FuncEq SometimeBefore where
    funcEq (SometimeBefore e11 e12 ) (SometimeBefore e21 e22) = (e11 == e21) && (e12 == e22)
eSometimeBefore e1 e2 = inject (SometimeBefore e1 e2)

data AlwaysWithin e = AlwaysWithin Double e e deriving (Data, Eq)
deriving instance Typeable1 AlwaysWithin
instance Functor AlwaysWithin where
    fmap f (AlwaysWithin d e1 e2) = AlwaysWithin d (f e1) (f e2)
instance FuncEq AlwaysWithin where
    funcEq (AlwaysWithin d1 e11 e12 ) (AlwaysWithin d2 e21 e22) = 
        (d1 == d2) && (e11 == e21) && (e12 == e22)
eAlwaysWithin d e1 e2 = inject (AlwaysWithin d e1 e2)

data HoldDuring e = HoldDuring Double Double e deriving (Data, Eq)
deriving instance Typeable1 HoldDuring
instance Functor HoldDuring where
    fmap f (HoldDuring b e p) = HoldDuring b e $ f p
instance FuncEq HoldDuring where
    funcEq (HoldDuring b1 e1 p1) (HoldDuring b2 e2 p2) = 
        (b1 == b2) && (e1 == e2) && (p1 == p2)
eHoldDuring b e p = inject (HoldDuring b e p)


data HoldAfter e = HoldAfter Double e deriving (Data, Eq)
deriving instance Typeable1 HoldAfter
instance Functor HoldAfter where
    fmap f (HoldAfter n e) = HoldAfter n $ f e
instance FuncEq HoldAfter where
    funcEq (HoldAfter n1 e1) (HoldAfter n2 e2) = (n1 == n2) && (e1 == e2)
eHoldAfter d e = inject (HoldAfter d e)



----------------------------------
-- Data instance derivations
----------------------------------
deriving instance (Typeable1 a, Data (a (Expr a))) => Data (Expr a)
