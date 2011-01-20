{-# OPTIONS
 #-}
{-# LANGUAGE 
    DeriveDataTypeable,
    FlexibleContexts,
    FlexibleInstances,
    FunctionalDependencies,
    MultiParamTypeClasses,
    Rank2Types
  #-}

module Planning.Records where

import Data.Generics
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
unName :: Name -> String
unName (Name a) = a
class Data a => HasName a where
    getName :: a -> String
    getName = unName . fromJust . gfind
    setName :: String -> a -> a
    setName n r = fromJust $ greplace r (Name n)

newtype DomainName = DomainName String deriving (Eq, Show, Data, Typeable)
unDomainName :: DomainName -> String
unDomainName (DomainName a) = a
class Data a => HasDomainName a where
    getDomainName :: a -> String
    getDomainName = unDomainName . fromJust . gfind
    setDomainName :: String -> a -> a
    setDomainName n r = fromJust $ greplace r (DomainName n)

newtype Requirements = Requirements [String] deriving (Eq, Show, Data, Typeable)
unRequirements :: Requirements -> [String]
unRequirements (Requirements a) = a
class Data a => HasRequirements a where
    getRequirements :: a -> [String]
    getRequirements = unRequirements . fromJust . gfind
    setRequirements :: [String] -> a -> a
    setRequirements rl r = fromJust $ greplace r (Requirements rl)

data Types f = Types [f] deriving (Eq, Show, Data, Typeable)
unTypes :: Types f -> [f]
unTypes (Types a) = a
class (Data a, Typeable f) => HasTypes f a | a -> f where
    getTypes :: a -> [f]
    getTypes = unTypes . fromJust . gfind
    setTypes :: [f] -> a -> a
    setTypes pl r = fromJust $ greplace r (Types pl)

data Constants f = Constants [f] deriving (Eq, Show, Data, Typeable)
unConstants :: Constants f -> [f]
unConstants (Constants a) = a
class (Data a, Typeable f) => HasConstants f a | a -> f where
    getConstants :: a -> [f]
    getConstants = unConstants . fromJust . gfind
    setConstants :: [f] -> a -> a
    setConstants pl r = fromJust $ greplace r (Constants pl)

data Initial f = Initial [f] deriving (Eq, Show, Data, Typeable)
unInitial :: Initial f -> [f]
unInitial (Initial a) = a
class (Data a, Typeable f) => HasInitial f a | a -> f where
    getInitial :: a -> [f]
    getInitial = unInitial . fromJust . gfind
    setInitial :: [f] -> a -> a
    setInitial pl r = fromJust $ greplace r (Initial pl)


data Predicates f = Predicates [f] deriving (Eq, Show, Data, Typeable)
unPredicates :: Predicates f -> [f]
unPredicates (Predicates a) = a
class (Data a, Typeable f) => HasPredicates f a | a -> f where
    getPredicates :: a -> [f]
    getPredicates = unPredicates . fromJust . gfind
    setPredicates :: [f] -> a -> a
    setPredicates pl r = fromJust $ greplace r (Predicates pl)

data Functions f = Functions [f] deriving (Eq, Show, Data, Typeable)
unFunctions :: Functions f -> [f]
unFunctions (Functions a) = a
class (Data a, Typeable f) => HasFunctions f a | a -> f where
    getFunctions :: a -> [f]
    getFunctions = unFunctions . fromJust . gfind
    setFunctions :: [f] -> a -> a
    setFunctions pl r = fromJust $ greplace r (Functions pl)

data Constraints f = Constraints (Maybe f) deriving (Eq, Show, Data, Typeable)
unConstraints :: Constraints f -> Maybe f
unConstraints (Constraints a) = a
class (Data a, Typeable f) => HasConstraints f a | a -> f where
    getConstraints :: a -> Maybe f
    getConstraints = unConstraints . fromJust . gfind
    setConstraints :: Maybe f -> a -> a
    setConstraints pre r = fromJust $ greplace r (Constraints pre)

data Goal f = Goal (Maybe f) deriving (Eq, Show, Data, Typeable)
unGoal :: Goal f -> Maybe f
unGoal (Goal a) = a
class (Data a, Typeable f) => HasGoal f a | a -> f where
    getGoal :: a -> Maybe f
    getGoal = unGoal . fromJust . gfind
    setGoal :: Maybe f -> a -> a
    setGoal pre r = fromJust $ greplace r (Goal pre)

data Items f = Items [f] deriving (Eq, Show, Data, Typeable)
unItems :: Items f -> [f]
unItems (Items a) = a
class (Data a, Typeable f) => HasItems f a | a -> f where
    getItems :: a -> [f]
    getItems = unItems . fromJust . gfind
    setItems :: [f] -> a -> a
    setItems pl r = fromJust $ greplace r (Items pl)


data Parameters f = Parameters [f] deriving (Eq, Show, Data, Typeable)
unParameters :: Parameters f -> [f]
unParameters (Parameters a) = a
class (Data a, Typeable f) => HasParameters f a | a -> f where
    getParameters :: a -> [f]
    getParameters = unParameters . fromJust . gfind
    setParameters :: [f] -> a -> a
    setParameters pl r = fromJust $ greplace r (Parameters pl)

data Precondition f = Precondition (Maybe f) deriving (Eq, Show, Data, Typeable)
unPrecondition :: Precondition f -> Maybe f
unPrecondition (Precondition a) = a
class (Data a, Typeable f) => HasPrecondition f a | a -> f where
    getPrecondition :: a -> Maybe f
    getPrecondition = unPrecondition . fromJust . gfind
    setPrecondition :: Maybe f -> a -> a
    setPrecondition pre r = fromJust $ greplace r (Precondition pre)

data Effect f = Effect (Maybe f) deriving (Eq, Show, Data, Typeable)
unEffect :: Effect f -> Maybe f
unEffect (Effect a) = a
class (Data a, Typeable f) => HasEffect f a | a -> f where
    getEffect :: a -> Maybe f
    getEffect = unEffect . fromJust . gfind
    setEffect :: Maybe f -> a -> a
    setEffect eff r = fromJust $ greplace r (Effect eff)


