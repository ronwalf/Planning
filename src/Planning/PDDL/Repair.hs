module Planning.PDDL.Repair
where

import Control.Monad
import Data.Graph.Inductive
--import Data.Graph.Inductive.Query.MST
--import Data.Graph.Inductive.Query.SP
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Prelude hiding (and, const, not, or)
import qualified Prelude

import Planning.PDDL.Representation

--mkTypeTree :: [TypedConstExpr] -> (Gr String Int, [(Int, String)])

class Functor f => TypeFlatten f where
    typeFlatten :: f [Expr Const] -> [Expr Const]
instance (TypeFlatten f , TypeFlatten g ) => TypeFlatten (f :+: g ) where 
    typeFlatten (Inl x ) = typeFlatten x 
    typeFlatten (Inr y ) = typeFlatten y 
instance TypeFlatten Const where
    typeFlatten (Const c) = [const c]
instance TypeFlatten (Typed (Expr Const)) where
    typeFlatten (Typed (In (Const c)) (In (Const t))) = [const c, const t]

class Functor f => GetType a f where
    getType :: f (Maybe (a, Expr Const)) -> Maybe ( a, Expr Const)
instance (GetType a f, GetType a g) => GetType a (f :+: g) where
    getType (Inl x) = getType x
    getType (Inr y) = getType y
instance GetType a (Typed a) where
    getType (Typed x t) = Just (x, t)
instance GetType a Const where
    getType (Const c) = Nothing
instance GetType a Var where
    getType (Var v) = Nothing

mkTypeGraph ::  (TypeFlatten f, GetType (Expr Const) f) => 
    [Expr f] -> Gr (Expr Const) (Expr Const, Expr Const)
mkTypeGraph tl =
    let
        nodes = zip [0 ..] $ nub $ concatMap (foldExpr typeFlatten) tl
        edges = 
            [ (i, i, (c, c)) | (i, c) <- nodes ] ++
            [ (rlookup t1 nodes, rlookup t2 nodes, (t1, t2)) |
            (t1, t2) <- mapMaybe (foldExpr getType) tl]
        --tg = mkGraph nodes edges :: Gr (Expr Const) (Expr Const, Expr Const)
    in
    mkGraph nodes edges
    where
        rlookup i ((f,s) : l) = if ( i == s) then f else rlookup i l
        --rlookup i = fst . fromJust . (find (\(f, s) -> i == s))

--leastGeneralType :: (DynGraph gr, Eq a, Show a) => gr a (a, a) -> LNode a -> LNode a -> Maybe a
leastGeneralType :: [TypedConstExpr] -> Expr Const -> Expr Const -> Maybe (Expr Const)
leastGeneralType tl t1 t2 =
    let 
        tg = mkTypeGraph tl
        consts = concatMap (foldExpr typeFlatten) tl
        n1 = nodeLookup tg t1
        n2 = nodeLookup tg t2
        LP shortest = lesp n1 n2 $ undir tg
        --rshortest = foldl (\ l (e, (i, o)) -> (e, (o, i)) : l) [] $ tail shortest
        rshortest = reverse $ tail shortest
        leftTop = foldl followOut t1 $ tail shortest
        rightTop = foldl followOut t2 rshortest
    in
    if (t1 == t2) then Just t1 else
    if (Prelude.not (t1 `elem` consts && t2 `elem` consts)) then Nothing else
    if shortest == [] then Nothing else
    if leftTop == rightTop then
        Just rightTop
    else
        Nothing
        --Just $ const (show leftTop ++ "-" ++ show rightTop)
        --Just $ const $ show (tail shortest, rshortest)
    where
        followOut current (_, (prev, next))
            | prev == current = next
            | otherwise = current
        nodeLookup tg t =
            fst $ fromJust $ find (\(n,s) -> s==t) $ labNodes tg

findTreeViolators tg =
    [(n1, fromJust $ lab tg i2) | (i1, n1) <- labNodes tg, (outdeg tg i1) > 1, (i2, _) <- lsuc tg i1]


repairDomain domain = do
    let tg = mkTypeGraph $ types domain
    when (hasLoop tg) $ fail "Type loop present"
    let violators = findTreeViolators tg
    when (length violators > 0) $ fail $ "Supertypes violate tree property: " ++ (show violators)
    return domain


