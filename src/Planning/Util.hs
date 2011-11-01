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
import Data.Maybe
import Control.Monad (liftM)

import Planning.Expressions

class (Functor f, Functor g) => LiftExpression f g where
    liftE' :: f (Expr g) -> Expr g
liftE :: (LiftExpression f g) => Expr f -> Expr g
liftE = foldExpr liftE'

instance (LiftExpression f h, LiftExpression g h) => LiftExpression (f :+: g) h where
    liftE' (Inl x) = liftE' x
    liftE' (Inr y) = liftE' y

instance (Var :<: g) => LiftExpression Var g where
    liftE' (Var v) = eVar v
instance (Const :<: g) => LiftExpression Const g where
    liftE' (Const g) = eConst g


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
    litName' :: f String -> String
litName :: LitName f => Expr f -> String
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
class (Functor f) => TermSubstitution t f g where
    substituteTerm' :: [(Expr Var, t)] -> f (Expr g) -> Expr g
substituteTerm :: TermSubstitution t f f => [(Expr Var, t)] -> Expr f -> Expr f
substituteTerm vsl (In e) = substituteTerm' vsl e

instance (TermSubstitution t f h, TermSubstitution t g h) => TermSubstitution t (f :+: g) h where
    substituteTerm' vsl (Inl x) = substituteTerm' vsl x
    substituteTerm' vsl (Inr y) = substituteTerm' vsl y

instance (Var :<: t) => TermSubstitution (Expr t) Var t where
    substituteTerm' vsl (Var v) = 
        fromMaybe (eVar v) $ 
        lookup (eVar v :: Expr Var) vsl

instance (Const :<: f) => TermSubstitution t Const f where
    substituteTerm' _ (Const c) = eConst c

instance (Function :<: f, TermSubstitution t f f) => TermSubstitution t Function f where
    substituteTerm' vsl (Function p tl) = eFunc p $ map (substituteTerm vsl) tl

instance (Typed (Expr d) :<: f, TermSubstitution t d d, TermSubstitution t f f) => TermSubstitution t (Typed (Expr d)) f where
    substituteTerm' vsl (Typed d t) = eTyped (substituteTerm vsl d) t

instance (Atomic (Expr t) :<: f, TermSubstitution (Expr t) t t) => TermSubstitution (Expr t) (Atomic (Expr t)) f where
    substituteTerm' vsl (Atomic p tl) = eAtomic p $ map (substituteTerm vsl) tl

instance (And :<: f, TermSubstitution t f f) => TermSubstitution t And f where
    substituteTerm' vsl (And el) = eAnd $ map (substituteTerm vsl) el
        
instance (Or :<: f, TermSubstitution t f f) => TermSubstitution t Or f where
    substituteTerm' vsl (Or el) = eOr $ map (substituteTerm vsl) el

instance (Not :<: f, TermSubstitution t f f) => TermSubstitution t Not f where
    substituteTerm' vsl (Not e) = eNot $ substituteTerm vsl e

instance (ForAll TypedVarExpr :<: f, TermSubstitution t f f) => TermSubstitution t (ForAll TypedVarExpr) f where
    substituteTerm' vsl (ForAll vl e) =
        let vsl' = filter (\(v, _) -> not $ v `elem` map removeType vl) vsl in
        eForAll vl $ substituteTerm vsl' e

instance (Exists TypedVarExpr :<: f, TermSubstitution t f f) => TermSubstitution t (Exists TypedVarExpr) f where
    substituteTerm' vsl (Exists vl e) =
        let vsl' = filter (\(v, _) -> not $ v `elem` map removeType vl) vsl in
        eExists vl $ substituteTerm vsl' e

instance (Imply :<: f, TermSubstitution t f f) => TermSubstitution t Imply f where
    substituteTerm' vsl (Imply e1 e2) = eImply (substituteTerm vsl e1) (substituteTerm vsl e2)

instance (Preference :<: f, TermSubstitution t f f) => TermSubstitution t Preference f where
    substituteTerm' vsl (Preference n e) = ePreference n $ substituteTerm vsl e

instance (When (Expr p) :<: f, TermSubstitution t p p, TermSubstitution t f f) => TermSubstitution t (When (Expr p)) f where
    substituteTerm' vsl (When p e) = eWhen (substituteTerm vsl p) (substituteTerm vsl e)


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
    cfConversion' (Var v) = fail $ "Cannot convert: Variable " ++ v ++ " found"
instance (Const :<: g) => CFConversion g Const where
    cfConversion' (Const c) = return $ eConst c
instance (Function :<: g) => CFConversion g Function where
    cfConversion' (Function f tl) = do
        gtl <- sequence tl
        return $ eFunc f gtl
instance (Atomic (Expr ct) :<: g, CFConversion ct t) => CFConversion g (Atomic (Expr t)) where
    cfConversion' (Atomic p tl) = do
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

