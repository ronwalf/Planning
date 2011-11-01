{-# LANGUAGE FlexibleInstances #-}
module Planning.Test (tests) where

import Distribution.TestSuite
import Test.QuickCheck

import qualified Planning.PDDL.ParserTest as ParserTest


tests = ParserTest.tests
