{-# LANGUAGE
    FlexibleContexts,
    FlexibleInstances,
    OverloadedStrings,
    ScopedTypeVariables,
    TypeSynonymInstances
    #-}
module Planning.PDDL.ParserTest (tests) where

import Control.Monad (liftM)
import qualified Data.Text as T
import Data.Word
import Test.QuickCheck
import qualified Test.QuickCheck.Property as Prop
import Text.Parsec (runParser)
import qualified Text.Parsec.Token as P
import Text.ParserCombinators.Parsec (CharParser)

import Planning.PDDL.Parser
import Planning.PDDL.PDDL3_0

-- Generate random identifiers
aname :: Char -> Gen [Char]
aname c = liftM ((c :) . show) (arbitrary :: Gen Word8)
tname :: Gen String
tname = aname 't'

newtype TypesList = TypesList [TypedTypeExpr]
-- Show instance for TypedTypeExpr
instance Show TypesList where
    show (TypesList tl) = show $ pddlDoc tl

-- Generate random types
instance Arbitrary TypesList where
    arbitrary = liftM (TypesList . concat) $ listOf (sized typelist)
        where
        typelist :: Int -> Gen [TypedTypeExpr]
        typelist n = do
            (cl :: [T.Text]) <- liftM (map T.pack) $ vectorOf n tname
            tn <- choose (0, 3 :: Int)
            (tl :: [T.Text]) <- liftM (map T.pack) $ vectorOf tn tname
            return $ map (flip eTyped tl) cl

-- TypedTypeExpr round tripping.
parseCheck :: (Eq o, PDDLDoc o) => CharParser () o -> String -> o -> Prop.Result
parseCheck parser input expected = case runParser parser () "" input of
    Left e -> Prop.failed { Prop.reason = show e }
    Right out -> if (out == expected) then Prop.succeeded else
        Prop.failed { Prop.reason = "Output <" ++ show (pddlDoc out) ++ "> doesn't match."}


prop_typedTypeRT :: TypesList -> Prop.Result
prop_typedTypeRT (TypesList t) = parseCheck
    (parseTypedList pddlExprLexer (liftM T.pack $ P.identifier pddlExprLexer))
    (show $ pddlDoc t)
    t

tests :: [TypesList -> Prop.Result]
tests = [ prop_typedTypeRT ]
