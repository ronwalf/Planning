{-# OPTIONS
 -fglasgow-exts
 #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, DeriveDataTypeable  #-}

module Planning.Records where

import Data.Generics
import Data.Generics.Aliases
import Data.Generics.Schemes
import Control.Monad
import Data.Maybe

-- Thank you, s.clover

greplace :: (Data a, Typeable b) => a -> b -> Maybe a
greplace x y = once (const Nothing `extM` (const (Just y))) x

once :: MonadPlus m => GenericM m -> GenericM m
once f x = f x `mplus` gmapMo (once f) x

gfind :: (Data a, Typeable b) => a -> Maybe b
gfind = something (const Nothing `extQ` Just)


newtype Name = Name String deriving (Eq, Show, Data, Typeable)
unName (Name a) = a
class Data a => HasName a where
    getName :: a -> String
    getName = unName . fromJust . gfind
    setName :: String -> a -> a
    setName n r = fromJust $ greplace r (Name n)

newtype Requirements = Requirements [String] deriving (Eq, Show, Data, Typeable)
unRequirements (Requirements a) = a
class Data a => HasRequirements a where
    getRequirements :: a -> [String]
    getRequirements = unRequirements . fromJust . gfind
    setRequirements :: [String] -> a -> a
    setRequirements rl r = fromJust $ greplace r (Requirements rl)

data Parameters f = Parameters [f] deriving (Eq, Show, Data, Typeable)
unParameters (Parameters a) = a
class (Data a, Typeable f) => HasParameters f a | a -> f where
    getParameters :: a -> [f]
    getParameters = unParameters . fromJust . gfind
    setParameters :: [f] -> a -> a
    setParameters pl r = fromJust $ greplace r (Parameters pl)

data Precondition f = Precondition (Maybe f) deriving (Eq, Show, Data, Typeable)
unPrecondition (Precondition a) = a
class (Data a, Typeable f) => HasPrecondition f a | a -> f where
    getPrecondition :: a -> Maybe f
    getPrecondition = unPrecondition . fromJust . gfind
    setPrecondition :: Maybe f -> a -> a
    setPrecondition pre r = fromJust $ greplace r (Precondition pre)

data Effect f = Effect (Maybe f) deriving (Eq, Show, Data, Typeable)
unEffect (Effect a) = a
class (Data a, Typeable f) => HasEffect f a | a -> f where
    getEffect :: a -> Maybe f
    getEffect = unEffect . fromJust . gfind
    setEffect :: Maybe f -> a -> a
    setEffect eff r = fromJust $ greplace r (Effect eff)


