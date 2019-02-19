{-# LANGUAGE
    DeriveDataTypeable,
    FlexibleContexts,
    FlexibleInstances,
    FunctionalDependencies,
    MultiParamTypeClasses,
    OverloadedStrings,
    StandaloneDeriving,
    TypeOperators,
    UndecidableInstances
  #-}

module Planning.Expressions (
    module Planning.Wouter,

    -- Terms
    Const(..), eConst,
    Var(..), eVar,
    Function(..), eFunc,

    -- Typing
    Typed(..), TypedExpression(..),
    TypedTypeExpr,
    TypedConst,
    TypedConstExpr,
    TypedVar,
    TypedVarExpr,
    TypedPredicate,
    TypedPredicateExpr,
    TypedFuncSkel,
    TypedFuncSkelExpr,
    Untypeable, removeType,
    GetType, getType,

    -- First Order Logic
    Atomic(..), AtomicExpression(..),
    And(..), eAnd,
    Imply(..), eImply,
    Not(..), eNot,
    Or(..), eOr,

    Exists(..), ExistsExpression(..),
    ForAll(..), ForAllExpression(..),

    When(..), WhenExpression(..),

    -- Preferences
    Preference(..), ePreference,
    unPreference,

    -- Timing
    Start(..), eStart,
    End(..), eEnd,
    All(..), eAll,
    At(..), AtExpression(..),
    Over(..), OverExpression(..),
    Always(..), eAlways,
    Sometime(..), eSometime,
    Within(..), eWithin,
    AtMostOnce(..), eAtMostOnce,
    SometimeAfter(..), eSometimeAfter,
    SometimeBefore(..), eSometimeBefore,
    AlwaysWithin(..), eAlwaysWithin,
    HoldDuring(..), eHoldDuring,
    HoldAfter(..), eHoldAfter,

    -- Partial Observability
    OneOf(..), eOneOf,
    Unknown(..), eUnknown
) where

import Data.Data
import Data.Text (Text)

import Planning.Wouter

---------------------------------
-- Term Holders
---------------------------------
data Const e = Const Text deriving (Data, Eq, Typeable)
instance Functor Const where
    fmap _ (Const x) = Const x
instance FuncEq Const where
    funcEq (Const x) (Const y) = x == y
instance FuncOrd Const where
    funcCompare (Const x) (Const y) = compare x y
eConst :: (Const :<: f) => Text -> Expr f
eConst x = inject (Const x)

data Var e = Var Text deriving (Data, Eq, Typeable)
instance Functor Var where
    fmap _ (Var x) = Var x
instance FuncEq Var where
    funcEq (Var x) (Var y) = x == y
instance FuncOrd Var where
    funcCompare (Var x) (Var y) = compare x y

eVar :: (Var :<: f) => Text -> Expr f
eVar x = inject (Var x)

data Function e = Function Text [e] deriving (Data, Eq, Typeable)
instance Functor Function where
    fmap f (Function n tl) = Function n $ map f tl
instance FuncEq Function where
    funcEq (Function n1 tl1) (Function n2 tl2) = (n1 == n2) && (tl1 == tl2)
instance FuncOrd Function where
    funcCompare (Function n1 tl1) (Function n2 tl2) =
        compare (n1, tl1) (n2, tl2)
eFunc :: (Function :<: f) => Text -> [Expr f] -> Expr f
eFunc n tl = inject (Function n tl)

---------------------------------------
-- Typing (commonly used to type Terms)
---------------------------------------

data Typed t e = Typed t [Text] deriving (Data, Eq, Typeable)
instance Functor (Typed t) where
    fmap _ (Typed e t) = Typed e t
instance Eq t => FuncEq (Typed t) where
    funcEq (Typed e1 t1) (Typed e2 t2) = (e1 == e2) && (t1 == t2)
instance Ord t => FuncOrd (Typed t) where
    funcCompare (Typed e1 t1) (Typed e2 t2) = compare (e1, t1) (e2, t2)
class (Typed t :<: f) => TypedExpression t f where
    eTyped :: t -> [Text] -> Expr f
instance (Typed t :<:f) => TypedExpression t f where
    eTyped e t = inject (Typed e t)
type TypedTypeExpr = Expr (Typed Text)
type TypedConst = Typed (Expr Const)
type TypedConstExpr = Expr TypedConst
type TypedVar = Typed (Expr Var)
type TypedVarExpr = Expr TypedVar
type TypedPredicate = Atomic TypedVarExpr
type TypedPredicateExpr = Expr TypedPredicate
type TypedFuncSkel = Typed TypedPredicateExpr -- i.e., no nested funcs in skeleton
type TypedFuncSkelExpr = Expr TypedFuncSkel

class Functor f=> Untypeable f g | f -> g where
    untype :: f g -> g
instance (Untypeable f h, Untypeable g h) => Untypeable (f :+: g) h where
    untype (Inl x) = untype x
    untype (Inr y) = untype y
instance Untypeable (Typed e) e where
    untype (Typed e _) = e

removeType :: Untypeable f g => Expr f -> g
removeType = foldExpr untype

class (Functor f) => GetType f where
    getType' :: f [Text] -> [Text]
instance (GetType f, GetType g) => GetType (f :+: g) where
    getType' (Inl x) = getType' x
    getType' (Inr x) = getType' x
instance GetType (Typed t) where
    getType' (Typed _ tl) = tl
getType :: GetType f => Expr f -> [Text]
getType = foldExpr getType'

--------------------------------------------------------
-- Literals
--------------------------------------------------------

data Atomic t e = Atomic Text [t] deriving (Data, Eq, Typeable)
instance Functor (Atomic a) where
    fmap _ (Atomic p tl) = Atomic p tl
instance (Eq t) => FuncEq (Atomic t) where
    funcEq (Atomic p1 tl1) (Atomic p2 tl2) = (p1 == p2) && (tl1 == tl2)
instance (Ord t) => FuncOrd (Atomic t) where
    funcCompare (Atomic p1 tl1) (Atomic p2 tl2) =
        compare (p1, tl1) (p2, tl2)

class AtomicExpression t f where
    eAtomic :: Text -> [t] -> Expr f
instance (Atomic t :<: f) => AtomicExpression t f where
    eAtomic p tl = inject (Atomic p tl)

data Not e = Not e deriving (Data, Eq, Typeable)
instance Functor Not where
    fmap f (Not e) = Not $ f e
instance FuncEq Not where
    funcEq (Not x) (Not y) = x == y
instance FuncOrd Not where
    funcCompare (Not x) (Not y) = compare x y
eNot :: (Not :<: f) => Expr f -> Expr f
eNot e = inject (Not e)

---------------------------------------
-- First Order Logic Connectives
---------------------------------------
data And e = And [e] deriving (Data, Eq, Typeable)
instance Functor And where
    fmap f (And el) = And $ map f el
instance FuncEq And where
    funcEq (And el1) (And el2) = el1 == el2
instance FuncOrd And where
    funcCompare (And el1) (And el2) = compare el1 el2
eAnd :: (And :<: f) => [Expr f] -> Expr f
eAnd [e] = e
eAnd el = inject (And el)

data Or e = Or [e] deriving (Data, Eq, Typeable)
instance Functor Or where
    fmap f (Or el) = Or $ map f el
instance FuncEq Or where
    funcEq (Or el1) (Or el2) = el1 == el2
instance FuncOrd Or where
    funcCompare (Or el1) (Or el2) = compare el1 el2
eOr :: (Or :<: f) => [Expr f] -> Expr f
eOr [e] = e
eOr el = inject (Or el)

data Imply e = Imply e e deriving (Data, Eq, Typeable)
instance Functor Imply where
    fmap f (Imply e1 e2) = Imply (f e1) (f e2)
instance FuncEq Imply where
    funcEq (Imply x1 y1) (Imply x2 y2) = (x1 == x2) && (y1 == y2)
instance FuncOrd Imply where
    funcCompare (Imply x1 y1) (Imply x2 y2) =
        compare (x1, y1) (x2, y2)
eImply :: (Imply :<: f) => Expr f -> Expr f -> Expr f
eImply e1 e2 = inject (Imply e1 e2)

data ForAll v e = ForAll [v] e deriving (Data, Eq, Typeable)
instance Functor (ForAll vl) where
    fmap f (ForAll vl e) = ForAll vl $ f e
instance Eq v => FuncEq (ForAll v) where
    funcEq (ForAll vl1 e1) (ForAll vl2 e2) = (vl1 == vl2) && (e1 == e2)
instance Ord v => FuncOrd (ForAll v) where
    funcCompare (ForAll vl1 e1) (ForAll vl2 e2) =
        compare (vl1, e1) (vl2, e2)
class ForAllExpression v f where
    eForAll :: [v] -> Expr f -> Expr f
instance (ForAll v :<: f) => ForAllExpression v f where
    eForAll [] e = e
    eForAll vl e = inject (ForAll vl e)

data Exists v e = Exists [v] e deriving (Data, Eq, Typeable)
instance Functor (Exists vl) where
    fmap f (Exists vl e) = Exists vl $ f e
instance Eq v => FuncEq (Exists v) where
    funcEq (Exists vl1 e1) (Exists vl2 e2) = (vl1 == vl2) && (e1 == e2)
instance Ord v => FuncOrd (Exists v) where
    funcCompare (Exists vl1 e1) (Exists vl2 e2) =
        compare (vl1, e1) (vl2, e2)
class ExistsExpression v f where
    eExists :: [v] -> Expr f -> Expr f
instance (Exists v :<: f) => ExistsExpression v f where
    eExists [] e = e
    eExists vl e = inject (Exists vl e)


data When p e = When p e deriving (Data, Eq, Typeable)
instance Functor (When p) where
    fmap f (When p e) = When p $ f e
instance Eq p => FuncEq (When p) where
    funcEq (When p1 e1) (When p2 e2) = (p1 == p2) && (e1 == e2)
instance Ord p => FuncOrd (When p) where
    funcCompare (When p1 e1) (When p2 e2) =
        compare (p1, e1) (p2, e2)
class WhenExpression p f where
    eWhen :: p -> Expr f -> Expr f
instance (When p :<: f) => WhenExpression p f where
    eWhen p e = inject (When p e)

----------------------------------
-- Preferences
----------------------------------
data Preference e = Preference (Maybe Text) e deriving (Data, Eq, Typeable)
instance Functor Preference where
    fmap f (Preference n e) = Preference n $ f e
instance FuncEq Preference where
    funcEq (Preference n1 e1) (Preference n2 e2) = (n1 == n2) && (e1 == e2)
instance FuncOrd Preference where
    funcCompare (Preference n1 e1) (Preference n2 e2) =
        compare (n1, e1) (n2, e2)
ePreference :: (Preference :<: f) => Maybe Text -> Expr f -> Expr f
ePreference n e = inject (Preference n e)

class Functor f => UnPreference g f where
    unPreference :: f (Maybe g) -> Maybe g


----------------------------------
-- Timing
----------------------------------
data Start e = Start deriving (Data, Eq, Typeable)
instance Functor Start where
    fmap _ Start = Start
instance FuncEq Start where
    funcEq _ _ = True
instance FuncOrd Start where
    funcCompare _ _ = EQ
eStart :: (:<:) Start f => Expr f
eStart = inject Start

data End e = End deriving (Data, Eq, Typeable)
instance Functor End where
    fmap _ End = End
instance FuncEq End where
    funcEq _ _ = True
instance FuncOrd End where
    funcCompare _ _ = EQ
eEnd :: (:<:) End f => Expr f
eEnd = inject End

data All e = All deriving (Data, Eq, Typeable)
instance Functor All where
    fmap _ All = All
instance FuncEq All where
    funcEq _ _ = True
instance FuncOrd All where
    funcCompare _ _ = EQ
eAll :: (:<:) All f => Expr f
eAll = inject All


data At t e = At t e deriving (Data, Eq, Typeable)
instance Functor (At t) where
    fmap f (At t e) = At t $ f e
instance Eq t => FuncEq (At t) where
    funcEq (At t1 e1) (At t2 e2) = (t1 == t2) && (e1 == e2)
instance Ord t => FuncOrd (At t) where
    funcCompare (At t1 e1) (At t2 e2) =
        compare (t1, e1) (t2, e2)
class AtExpression t f where
    eAt :: t -> Expr f -> Expr f
instance (At t :<: f) => AtExpression t f where
    eAt t e = inject (At t e)

data Over t e = Over t e deriving (Data, Eq, Typeable)
instance Functor (Over t) where
    fmap f (Over t e) = Over t $ f e
instance Eq t => FuncEq (Over t) where
    funcEq (Over t1 e1) (Over t2 e2) = (t1 == t2) && (e1 == e2)
instance Ord t => FuncOrd (Over t) where
    funcCompare (Over t1 e1) (Over t2 e2) =
        compare (t1, e1) (t2, e2)
class OverExpression t f where
    eOver :: t -> Expr f -> Expr f
instance (Over t :<: f) => OverExpression t f where
    eOver t e = inject (Over t e)

data Always e = Always e deriving (Data, Eq, Typeable)
instance Functor Always where
    fmap f (Always e) = Always $ f e
instance FuncEq Always where
    funcEq (Always e1) (Always e2) = e1 == e2
instance FuncOrd Always where
    funcCompare (Always e1) (Always e2) = compare e1 e2
eAlways :: (Always :<: f) => Expr f -> Expr f
eAlways e = inject (Always e)

data Sometime e = Sometime e deriving (Data, Eq, Typeable)
instance Functor Sometime where
    fmap f (Sometime e) = Sometime $ f e
instance FuncEq Sometime where
    funcEq (Sometime e1) (Sometime e2) = e1 == e2
instance FuncOrd Sometime where
    funcCompare (Sometime e1) (Sometime e2) = compare e1 e2
eSometime :: (Sometime :<: f) => Expr f -> Expr f
eSometime e = inject (Sometime e)

data Within e = Within Double e deriving (Data, Eq, Typeable)
instance Functor Within where
    fmap f (Within n e) = Within n $ f e
instance FuncEq Within where
    funcEq (Within n1 e1) (Within n2 e2) = (n1 == n2) && (e1 == e2)
instance FuncOrd Within where
    funcCompare (Within n1 e1) (Within n2 e2) =
        compare (n1, e1) (n2, e2)
eWithin :: (Within :<: f) => Double -> Expr f -> Expr f
eWithin d e = inject (Within d e)

data AtMostOnce e = AtMostOnce e deriving (Data, Eq, Typeable)
instance Functor AtMostOnce where
    fmap f (AtMostOnce e) = AtMostOnce $ f e
instance FuncEq AtMostOnce where
    funcEq (AtMostOnce e1) (AtMostOnce e2) = e1 == e2
instance FuncOrd AtMostOnce where
    funcCompare (AtMostOnce e1) (AtMostOnce e2) =
        compare e1 e2
eAtMostOnce :: (AtMostOnce :<: f) => Expr f -> Expr f
eAtMostOnce e = inject (AtMostOnce e)

data SometimeAfter e = SometimeAfter e e deriving (Data, Eq, Typeable)
instance Functor SometimeAfter where
    fmap f (SometimeAfter e1 e2) = SometimeAfter (f e1) (f e2)
instance FuncEq SometimeAfter where
    funcEq (SometimeAfter e11 e12 ) (SometimeAfter e21 e22) = (e11 == e21) && (e12 == e22)
instance FuncOrd SometimeAfter where
    funcCompare (SometimeAfter e11 e12 ) (SometimeAfter e21 e22) =
        compare (e11, e12) (e21, e22)
eSometimeAfter :: (SometimeAfter :<: f) => Expr f -> Expr f -> Expr f
eSometimeAfter e1 e2 = inject (SometimeAfter e1 e2)

data SometimeBefore e = SometimeBefore e e deriving (Data, Eq, Typeable)
instance Functor SometimeBefore where
    fmap f (SometimeBefore e1 e2) = SometimeBefore (f e1) (f e2)
instance FuncEq SometimeBefore where
    funcEq (SometimeBefore e11 e12 ) (SometimeBefore e21 e22) = (e11 == e21) && (e12 == e22)
instance FuncOrd SometimeBefore where
    funcCompare (SometimeBefore e11 e12 ) (SometimeBefore e21 e22) =
        compare (e11, e12) (e21, e22)
eSometimeBefore :: (SometimeBefore :<: f) => Expr f -> Expr f -> Expr f
eSometimeBefore e1 e2 = inject (SometimeBefore e1 e2)

data AlwaysWithin e = AlwaysWithin Double e e deriving (Data, Eq, Typeable)
instance Functor AlwaysWithin where
    fmap f (AlwaysWithin d e1 e2) = AlwaysWithin d (f e1) (f e2)
instance FuncEq AlwaysWithin where
    funcEq (AlwaysWithin d1 e11 e12 ) (AlwaysWithin d2 e21 e22) =
        (d1 == d2) && (e11 == e21) && (e12 == e22)
instance FuncOrd AlwaysWithin where
    funcCompare (AlwaysWithin d1 e11 e12 ) (AlwaysWithin d2 e21 e22) =
        compare (d1, e11, e12) (d2, e21, e22)
eAlwaysWithin :: (AlwaysWithin :<: f) => Double -> Expr f -> Expr f -> Expr f
eAlwaysWithin d e1 e2 = inject (AlwaysWithin d e1 e2)

data HoldDuring e = HoldDuring Double Double e deriving (Data, Eq, Typeable)
instance Functor HoldDuring where
    fmap f (HoldDuring b e p) = HoldDuring b e $ f p
instance FuncEq HoldDuring where
    funcEq (HoldDuring b1 e1 p1) (HoldDuring b2 e2 p2) =
        (b1 == b2) && (e1 == e2) && (p1 == p2)
instance FuncOrd HoldDuring where
    funcCompare (HoldDuring b1 e1 p1) (HoldDuring b2 e2 p2) =
        compare (b1, e1, p1) (b2, e2, p2)
eHoldDuring :: (HoldDuring :<: f) => Double -> Double -> Expr f -> Expr f
eHoldDuring b e p = inject (HoldDuring b e p)


data HoldAfter e = HoldAfter Double e deriving (Data, Eq, Typeable)
instance Functor HoldAfter where
    fmap f (HoldAfter n e) = HoldAfter n $ f e
instance FuncEq HoldAfter where
    funcEq (HoldAfter n1 e1) (HoldAfter n2 e2) = (n1 == n2) && (e1 == e2)
instance FuncOrd HoldAfter where
    funcCompare (HoldAfter n1 e1) (HoldAfter n2 e2) =
        compare (n1, e1) (n2, e2)
eHoldAfter :: (HoldAfter :<: f) => Double -> Expr f -> Expr f
eHoldAfter d e = inject (HoldAfter d e)


----------------------------------
-- Partial Observability
----------------------------------
data OneOf e = OneOf [e] deriving (Data, Eq, Typeable)
instance Functor OneOf where
    fmap f (OneOf e) = OneOf $ map f e
instance FuncEq OneOf where
    funcEq (OneOf e1) (OneOf e2) = e1 == e2
instance FuncOrd OneOf where
    funcCompare (OneOf e1) (OneOf e2) =
        compare e1 e2
eOneOf :: (OneOf :<: f) => [Expr f] -> Expr f
eOneOf e = inject (OneOf e)

data Unknown e = Unknown e deriving (Data, Eq, Typeable)
instance Functor Unknown where
    fmap f (Unknown e) = Unknown $ f e
instance FuncEq Unknown where
    funcEq (Unknown e1) (Unknown e2) = e1 == e2
instance FuncOrd Unknown where
    funcCompare (Unknown e1) (Unknown e2) =
        compare e1 e2
eUnknown :: (Unknown :<: f) => Expr f -> Expr f
eUnknown e = inject (Unknown e)
