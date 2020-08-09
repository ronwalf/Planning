{-# LANGUAGE
    AllowAmbiguousTypes,
    FlexibleContexts,
    FlexibleInstances,
    FunctionalDependencies,
    MultiParamTypeClasses,
    ScopedTypeVariables,
    TypeOperators,
    UndecidableInstances
  #-}
module Planning.Util where

import Control.Monad (liftM)
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

import Planning.Expressions

class (Functor f, Functor g) => LiftExpression f g where
    liftE' :: f (Expr g) -> Expr g
liftE :: (LiftExpression f g) => Expr f -> Expr g
liftE = foldExpr liftE'

instance (LiftExpression f h, LiftExpression g h) => LiftExpression (f :+: g) h where
    liftE' (Inl x) = liftE' x
    liftE' (Inr y) = liftE' y

instance (Functor g, Var :<: g) => LiftExpression Var g where
    liftE' (Var v) = eVar v
instance (Functor g, Const :<: g) => LiftExpression Const g where
    liftE' (Const g) = eConst g
instance (Functor g, Function :<: g) => LiftExpression Function g where
    liftE' (Function p tl) = eFunc p tl
instance (Functor g, Not :<: g) => LiftExpression Not g where
    liftE' (Not e) = eNot e
instance (Functor g, Atomic e :<: g) => LiftExpression (Atomic e) g where
    liftE' (Atomic p tl) = eAtomic p tl

class (Functor f) => IsPosLit f where
    isPosLit' :: f Bool -> Bool
isPosLit :: (IsPosLit f) => Expr f -> Bool
isPosLit = foldExpr isPosLit'

instance (IsPosLit f, IsPosLit g) => IsPosLit (f :+: g) where
    isPosLit' (Inr x) = isPosLit' x
    isPosLit' (Inl y) = isPosLit' y
instance IsPosLit (Atomic t) where
    isPosLit' _ = True
instance IsPosLit Not where
    isPosLit' (Not x) = not x

class (Functor f) => LitName f where
    litName' :: f Text -> Text
litName :: LitName f => Expr f -> Text
litName = foldExpr litName'

instance (LitName f, LitName g) => LitName (f :+: g) where
    litName' (Inr x) = litName' x
    litName' (Inl y) = litName' y
instance LitName (Atomic t) where
    litName' (Atomic p _) = p
instance LitName Not where
    litName' (Not x) = x

class (Functor f) => LitArgs t f where
    litArgs' :: f [t] -> [t]
litArgs :: LitArgs t f => Expr f -> [t]
litArgs = foldExpr litArgs'

instance (LitArgs t f, LitArgs t g) => LitArgs t (f :+: g) where
    litArgs' (Inr x) = litArgs' x
    litArgs' (Inl y) = litArgs' y
instance LitArgs t (Atomic t) where
    litArgs' (Atomic _ tl) = tl
instance LitArgs t Not where
    litArgs' (Not tl) = tl

-- nnf helper class
-- nnf' takes a bool arg, False for negated, True for not-negated.
class (Functor f, Functor g) => NNF f g where
    nnf' :: Bool -> f (Expr g) -> Expr g
nnf :: (NNF f f) => Expr f -> Expr f
nnf (In e) = nnf' True e

instance (Functor g, NNF f h, NNF g h) => NNF (f :+: g) h where
    nnf' b (Inr x) = nnf' b x
    nnf' b (Inl y) = nnf' b y

instance (Functor g, (:<:) Not g, (:<:) (Atomic t) g) => NNF (Atomic t) g where
    nnf' True (Atomic p tl) = eAtomic p tl
    nnf' False (Atomic p tl) = eNot $ eAtomic p tl

instance (Functor g, NNF g g) => NNF Not g where
    nnf' b (Not (In e)) = nnf' (not b) e

instance (Functor g, (:<:) And g, (:<:) Or g, NNF g g) => NNF And g where
    nnf' True (And el) = eAnd [nnf' True e | In e <-  el]
    nnf' False (And el) = eOr [nnf' False e | In e <- el]

instance (Functor g, (:<:) And g, (:<:) Or g, NNF g g) => NNF Or g where
    nnf' True (Or el) = eOr [nnf' True e | In e <-  el]
    nnf' False (Or el) = eAnd [nnf' False e | In e <-  el]

instance (Functor g, (:<:) (Exists t) g, (:<:) (ForAll t) g, NNF g g) => NNF (Exists t) g where
    nnf' True (Exists vl (In e)) = eExists vl $ nnf' True e
    nnf' False (Exists vl (In e)) = eForAll vl $ nnf' False e

instance (Functor g, (:<:) (ForAll t) g, (:<:) (Exists t) g, NNF g g) => NNF (ForAll t) g where
    nnf' True (ForAll vl (In e)) = eForAll vl $ nnf' True e
    nnf' False (ForAll vl (In e)) = eExists vl $ nnf' False e

instance (Functor g, (:<:) And g, (:<:) Imply g, NNF g g) => NNF Imply g where
    nnf' True (Imply (In e1) (In e2)) = eImply (nnf' True e1) (nnf' True e2)
    nnf' False (Imply (In e1) (In e2)) = eAnd [nnf' True e1, nnf' False e2]

instance (Functor g, (:<:) Preference g, NNF g g) => NNF Preference g where
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

instance (Functor g, (:<:) (Atomic t) g) => Conjuncts (Atomic t) g where
    conjuncts' (Atomic p tl) = [eAtomic p tl]
instance (Functor g, (:<:) And g, Conjuncts g g) => Conjuncts And g where
    conjuncts' (And el) = concatMap conjuncts el
instance (Functor g, (:<:) Or g) => Conjuncts Or g where
    conjuncts' (Or el) = [eOr el]
instance (Functor g, (:<:) Not g) => Conjuncts Not g where
    conjuncts' (Not e) = [eNot e]
instance (Functor g, (:<:) (ForAll t) g) => Conjuncts (ForAll t) g where
    conjuncts' (ForAll vl e) = [eForAll vl e]
instance (Functor g, (:<:) (Exists t) g) => Conjuncts (Exists t) g where
    conjuncts' (Exists vl e) = [eExists vl e]
instance (Functor g, (:<:) Imply g) => Conjuncts Imply g where
    conjuncts' (Imply e1 e2) = [eImply e1 e2]
instance (Functor g, (:<:) Preference g) => Conjuncts Preference g where
    conjuncts' (Preference n e) = [ePreference n e]
instance (Functor g, (:<:) (When p) g) => Conjuncts (When p) g where
    conjuncts' (When p e) = [eWhen p e]
instance (Functor g, (:<:) OneOf g) => Conjuncts OneOf g where
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
instance AtomsFindable (At f) g where
    findAtoms' (At _ _) = []
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

-- Finds all literals (kinda assumes NNF form)
class (Functor f) => LiteralsFindable f g where
    findLiterals' :: f [Expr (Not :+: Atomic g)] -> [Expr (Not :+: Atomic g)]
findLiterals :: LiteralsFindable f g => Expr f -> [Expr (Not :+: Atomic g)]
findLiterals = foldExpr findLiterals'

instance (LiteralsFindable f g, LiteralsFindable h g) => LiteralsFindable (f :+: h) g where
    findLiterals' (Inl x) = findLiterals' x
    findLiterals' (Inr y) = findLiterals' y
instance LiteralsFindable (Atomic g) g where
    findLiterals' (Atomic p tl) = [eAtomic p tl]
instance LiteralsFindable And g where
    findLiterals' (And el) = concat el
instance LiteralsFindable Or g where
    findLiterals' (Or el) = concat el
instance LiteralsFindable Not g where
    findLiterals' (Not e) = map eNot e
instance LiteralsFindable (ForAll t) g where
    findLiterals' (ForAll _ e) = e
instance LiteralsFindable (Exists t) g where
    findLiterals' (Exists _ e) = e
instance LiteralsFindable Imply g where
    findLiterals' (Imply e1 e2) = e1 ++ e2
instance LiteralsFindable Preference g where
    findLiterals' (Preference _ e) = e
instance LiteralsFindable f g => LiteralsFindable (When (Expr f)) g where
    findLiterals' (When p e) = findLiterals p ++ e
instance LiteralsFindable OneOf g where
    findLiterals' (OneOf el) = concat el



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

-- Variable Subsitution
class (Functor f) => VarSubstitution t f g where
    substituteVar' :: Expr Var -> t -> f (Expr g) -> Expr g
substituteVar :: VarSubstitution t f f => Expr Var -> t -> Expr f -> Expr f
substituteVar v t (In e) = substituteVar' v t e
substituteVars :: VarSubstitution t f f => [(Expr Var, t)] -> Expr f -> Expr f
substituteVars vsl = flip (foldl (\e' (v, t) -> substituteVar v t e')) vsl

instance (VarSubstitution t f h, VarSubstitution t g h) => VarSubstitution t (f :+: g) h where
    substituteVar' v t (Inl x) = substituteVar' v t x
    substituteVar' v t (Inr y) = substituteVar' v t y

instance (Var :<: t) => VarSubstitution (Expr t) Var t where
    substituteVar' v t (Var v') = if eVar v' == v then t else eVar v'

instance (Const :<: f) => VarSubstitution t Const f where
    substituteVar' _ _ (Const c) = eConst c

instance (Function :<: f, VarSubstitution t f f) => VarSubstitution t Function f where
    substituteVar' v t (Function p tl) = eFunc p $ map (substituteVar v t ) tl

instance (Typed (Expr d) :<: f, VarSubstitution t d d, VarSubstitution t f f) => VarSubstitution t (Typed (Expr d)) f where
    substituteVar' v t (Typed d ty) = eTyped (substituteVar v t d) ty

instance (Atomic (Expr t) :<: f, VarSubstitution (Expr t) t t) => VarSubstitution (Expr t) (Atomic (Expr t)) f where
    substituteVar' v t (Atomic p tl) = eAtomic p $ map (substituteVar v t) tl

instance (And :<: f, VarSubstitution t f f) => VarSubstitution t And f where
    substituteVar' v t (And el) = eAnd $ map (substituteVar v t) el

instance (Or :<: f, VarSubstitution t f f) => VarSubstitution t Or f where
    substituteVar' v t (Or el) = eOr $ map (substituteVar v t) el

instance (Not :<: f, VarSubstitution t f f) => VarSubstitution t Not f where
    substituteVar' v t (Not e) = eNot $ substituteVar v t e

instance (ForAll TypedVarExpr :<: f, VarSubstitution t f f) => VarSubstitution t (ForAll TypedVarExpr) f where
    substituteVar' v t (ForAll vl e) = if v `elem` map removeType vl
        then eForAll vl e
            else eForAll vl $ substituteVar v t e

instance (Exists TypedVarExpr :<: f, VarSubstitution t f f) => VarSubstitution t (Exists TypedVarExpr) f where
    substituteVar' v t (Exists vl e) = if v `elem` map removeType vl
        then eExists vl e
            else eExists vl $ substituteVar v t e

instance (Imply :<: f, VarSubstitution t f f) => VarSubstitution t Imply f where
    substituteVar' v t (Imply e1 e2) = eImply (substituteVar v t e1) (substituteVar v t e2)

instance (Preference :<: f, VarSubstitution t f f) => VarSubstitution t Preference f where
    substituteVar' v t (Preference n e) = ePreference n $ substituteVar v t e

instance (When (Expr p) :<: f, VarSubstitution t p p, VarSubstitution t f f) => VarSubstitution t (When (Expr p)) f where
    substituteVar' v t (When p e) = eWhen (substituteVar v t p) (substituteVar v t e)


-- Check for constants (Not counting types)
class (Functor f) => FindConstants f where
    findConstants' :: f [Expr Const] -> [Expr Const]

findConstants :: (FindConstants f) => Expr f -> [Expr Const]
findConstants = foldExpr findConstants'

instance (FindConstants f, FindConstants g) => FindConstants (f :+: g) where
    findConstants' (Inl x) = findConstants' x
    findConstants' (Inr y) = findConstants' y

instance FindConstants Var where
    findConstants' _ = []
instance FindConstants Const where
    findConstants' (Const c) = [eConst c]
instance FindConstants Function where
    findConstants' (Function _ tl) = concat tl
instance (FindConstants t) => FindConstants (Typed (Expr t)) where
    findConstants' (Typed d _) = findConstants d

instance (FindConstants t) => FindConstants (Atomic (Expr t)) where
    findConstants' (Atomic _ tl) = concatMap findConstants tl
instance FindConstants And where
    findConstants' (And el) = concat el
instance FindConstants Or where
    findConstants' (Or el) = concat el
instance FindConstants Not where
    findConstants' (Not e) = e
instance FindConstants (ForAll v) where
    findConstants' (ForAll _ e) = e
instance FindConstants (Exists v) where
    findConstants' (Exists _ e) = e
instance FindConstants Imply where
    findConstants' (Imply e1 e2) = e1 ++ e2
instance FindConstants Preference where
    findConstants' (Preference _ e) = e
instance (FindConstants p) => FindConstants (When (Expr p)) where
    findConstants' (When p e) = findConstants p ++ e

-- Try to convert an expression to one with only constant and function terms
class (Functor f) => CFConversion g f where
    cfConversion' :: (Monad m) => f (m (Expr g)) -> m (Expr g)
cfConversion:: (Monad m, CFConversion g f) => Expr f -> m (Expr g)
cfConversion = foldExpr cfConversion'

instance (CFConversion h f, CFConversion h g) => CFConversion h (f :+: g) where
    cfConversion' (Inl x) = cfConversion' x
    cfConversion' (Inr y) = cfConversion' y

instance CFConversion g Var where
    cfConversion' (Var v) = error $ "Cannot convert: Variable " ++ (T.unpack v) ++ " found"
instance (Const :<: g) => CFConversion g Const where
    cfConversion' (Const c) = return $ eConst c
instance (Function :<: g) => CFConversion g Function where
    cfConversion' (Function f tl) = do
        gtl <- sequence tl
        return $ eFunc f gtl
instance (Atomic (Expr ct) :<: g, CFConversion ct t) => CFConversion g (Atomic (Expr t)) where
    cfConversion' (Atomic p (tl :: [Expr t])) = do
        ctl <- sequence $ map cfConversion tl
        return $ eAtomic p (ctl :: [Expr ct])
instance (Not :<: g) => CFConversion g Not where
    cfConversion' (Not e) = liftM eNot e
instance (And :<: g) => CFConversion g And where
    cfConversion' (And el) = liftM eAnd $ sequence el
instance (Or :<: g) => CFConversion g Or where
    cfConversion' (Or el) = liftM eOr $ sequence el
instance (At (Expr cg) :<: g, CFConversion cg c) => CFConversion g (At (Expr c)) where
    cfConversion' (At c e) = do
        cg <- cfConversion c
        e' <- e
        return $ eAt (cg :: Expr cg) e'


{- Unfinished
-- Return a var substitution list for unifying expr1 to expr 2
-- Returns
unify :: forall m t f .
    (Monad m
    , VarSubstitution (Expr t) f f, Var :<: t
    , FreeVarsFindable f
    ) => Expr f -> Expr f -> m [(Expr Var, Expr t)]
unify e1 e2 = do
    let vtl = renameVars (findFreeVars e1) (findFreeVars e2)
    let e1' = substituteVars vtl e1
    return undefined
    where
    renameVars :: [Expr Var] -> [Expr Var] -> [(Expr Var, Expr t)]
    renameVars [] l = []
    renameVars (v : vl) l
        | v `elem` l = let v' = newVarName v l in (v, liftE v' :: Expr t) : renameVars vl (v' : l)
        | otherwise = renameVars vl (v : l)
    newVarName :: Expr Var -> [Expr Var] -> Expr Var
    newVarName v@(In (Var vn)) l
        | v `elem` l = newVarName (eVar (vn ++ "_")) l
        | otherwise =  v
-}
