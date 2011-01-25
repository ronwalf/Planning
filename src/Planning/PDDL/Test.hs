module Planning.PDDL.Test where

import Data.List
import Prelude hiding (and, const, not, or)
import Test.HUnit
import Test.HUnit.Text

import Planning.PDDL.Representation
import Planning.PDDL.Repair

consts :: [Expr Const]
consts = [ const ('t' : show n) | n <- [0..9] ]

tl :: [TypedConstExpr]
tl = [
    typed (consts !! 0) (consts !! 1),
    typed (consts !! 2) (consts !! 3),
    typed (consts !! 1) (consts !! 4),
    typed (consts !! 3) (consts !! 4),
    const "t5" 
    ] 

related = take 5 consts
unrelated = drop 5 consts

ungeneralizableTypesTest tl t1 t2 = 
    TestLabel ("UngeneralizableTypesTest-" ++ (show t1) ++ "-" ++ (show t2)) $
    TestCase $ assertEqual "Unexpected generalized type!:" Nothing $
    leastGeneralType tl t1 t2

generalizableTypesTest tl t1 t2 te =
    TestLabel ("GeneralizableTypesTest-" ++ intercalate "-" (map show [t1, t2, te])) $
    TestCase $ assertEqual "Unexpected or unfound generalized type!:" (Just te) $
    leastGeneralType tl t1 t2

noRelationTests = TestLabel "Type Relation Tests" $ TestList $
    [ ungeneralizableTypesTest tl c1 c2 
        | c1 <- related ++ unrelated
        | c2 <- unrelated ++ related ] ++
    [ generalizableTypesTest tl c1 c2 (consts !! 4)
        | c1 <- related ++ repeat (consts !! 4)
        | c2 <- replicate (length related) (consts !! 4) ++ related ]

