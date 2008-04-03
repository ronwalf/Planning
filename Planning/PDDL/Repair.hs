module Planning.PDDL.Repair (
repairDomain
) where

import Control.Monad
import Data.Graph.Inductive
import Data.Graph.Inductive.Query.MST
import Data.Graph.Inductive.Query.SP
import Data.List
import qualified Data.Map as Map
import Data.Maybe

import Planning.PDDL.Representation

transformTL f = map (\ (s, t) -> (f s, t)) 

mkTypeTree :: [(String, Maybe String)] -> (Gr String Int, [(Int, String)])
mkTypeTree tl =
    let
        --edges = mapMaybe (\ (t1, mt2) -> if (isJust mt2) then Just (t1, fromJust mt2) else Nothing) tl
        nodes = zip [0 ..] $ nub $ catMaybes $ concatMap (\ (t1, mt2) -> [Just t1, mt2]) tl
        edges = [ (fst n1, fst n2) | n1 <- nodes, n2 <- nodes, (elem (snd n1, Just $ snd n2) tl) || (fst n1) == (fst n2)]
        ledges = zipWith (\ (x,y) w -> (x,y,w)) edges $ repeat 1
    in
    (mkGraph nodes ledges, nodes)

findTypeNum label nodes = 
    fst $ fromJust $ find (\ (n, l) -> label == l) nodes

findTypeStr num nodes =
    fromJust $ lookup num nodes

isSubType (tree, nodes) t1 t2 =
    not $ null $ map (flip lookup $ nodes) $ sp (findTypeNum t1 nodes) (findTypeNum t2 nodes) tree

isSuperType types = flip (isSubType types)

predEquality (p1, vl1) (p2, vl2) = 
    (p1 == p2) && (all similar $ zip vl1 vl2) 
    where
        similar ((_, Nothing), (_, _)) = True
        similar ((_, _), (_, Nothing)) = True
        similar ((_, t1), (_, t2)) = t1 == t2

findPreds typing (And cl) = concatMap (findPreds typing) cl
findPreds typing (Atomic p tl) = [(p, map (\t -> (t, fromMaybe Nothing $ lookup t typing)) tl )]
findPreds _ Empty = []
findPreds typing (Exists vl c) = findPreds ((transformTL Var vl) ++ typing) c
findPreds typing (ForAll vl c) = findPreds ((transformTL Var vl) ++ typing) c
findPreds typing (Imply c1 c2) = (findPreds typing c1) ++ (findPreds typing c2)
findPreds typing (Not c) = findPreds typing c
findPreds typing (Or cl)  = concatMap (findPreds typing) cl
findPreds typing (When c1 c2) = (findPreds typing c1) ++ (findPreds typing c2)


findActionPreds consts a@(Action { parameters = pl}) =
    let
        typing = (transformTL Const consts) ++ (transformTL Var pl)
    in
    (findPreds typing $ effect a) ++ (findPreds typing $ precondition a)

--findActionPreds _ _ = []


mostGeneralType types tl =
    foldl supplant (if (null tl) then Nothing else head tl) tl
    where 
        supplant Nothing _ = Nothing
        supplant _ Nothing = Nothing
        supplant t1 t2 = 
            if (isSubType types (fromJust t1) (fromJust t2)) then t2
            else if (isSubType types (fromJust t2) (fromJust t1)) then t1
            else Nothing

repairPredicate types (p, vls) = do
    let sizes = nub $ map length vls
    when (length sizes /= 1) (fail $ "Predicate " ++ p ++ " has multiple arities: " ++ show sizes)
    let byArgument = transpose vls
    let argNames = if (null vls) then [] else (map (termName . fst) $ head vls)
    let argTypes = map (mostGeneralType types) $ map (map snd) byArgument
    return (p, zip argNames argTypes)
    

notKnownFilter = filter notKnown where
    notKnown (p, args)
        | p == "=" && length args == 2 = False
        | otherwise = True


repairDomain (Domain (info, items)) = do
    
    let allPreds = reverse $ nubBy predEquality $ (map (\(p, vl) -> (p, transformTL Var vl)) $ predicates info) ++ concatMap (findActionPreds $ constants info) items
    let nameMap = foldr (\ (p, vl) -> Map.insertWith (++) p [vl]) Map.empty allPreds 
    let typeTree = mkTypeTree $ types info
    preds <- liftM (notKnownFilter) $ mapM (repairPredicate typeTree) $ Map.toList nameMap
    
    return $ Domain (info { predicates = preds }, items)
