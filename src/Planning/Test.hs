{-# LANGUAGE FlexibleInstances #-}
module Planning.Test (tests) where

import Test.QuickCheck

import qualified Planning.PDDL.ParserTest as ParserTest

tests = ParserTest.tests
