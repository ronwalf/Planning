{-# OPTIONS
 #-}
{-# LANGUAGE
    DeriveDataTypeable,
    FlexibleContexts,
    FlexibleInstances,
    FunctionalDependencies,
    MultiParamTypeClasses,
    OverloadedStrings,
    Rank2Types
  #-}

module Planning.Records where

import Data.Generics
import Control.Monad
import Data.Maybe
import Data.Text (Text)

-- Thank you, s.clover

greplace :: (Data a, Typeable b) => a -> b -> Maybe a
greplace x y = once (const Nothing `extM` (const (Just y))) x

once :: MonadPlus m => GenericM m -> GenericM m
once f x = f x `mplus` gmapMo (once f) x

gfind :: (Data a, Typeable b) => a -> Maybe b
gfind = something (const Nothing `extQ` Just)


newtype Name = Name Text deriving (Eq, Show, Data, Typeable)
unName :: Name -> Text
unName (Name a) = a
class Data a => HasName a where
    getName :: a -> Text
    getName = unName . fromJust . gfind
    setName :: Text -> a -> a
    setName n r = fromJust $ greplace r (Name n)

newtype DomainName = DomainName Text deriving (Eq, Show, Data, Typeable)
unDomainName :: DomainName -> Text
unDomainName (DomainName a) = a
class Data a => HasDomainName a where
    getDomainName :: a -> Text
    getDomainName = unDomainName . fromJust . gfind
    setDomainName :: Text -> a -> a
    setDomainName n r = fromJust $ greplace r (DomainName n)

newtype Requirements = Requirements [Text] deriving (Eq, Show, Data, Typeable)
unRequirements :: Requirements -> [Text]
unRequirements (Requirements a) = a
class Data a => HasRequirements a where
    getRequirements :: a -> [Text]
    getRequirements = unRequirements . fromJust . gfind
    setRequirements :: [Text] -> a -> a
    setRequirements rl r = fromJust $ greplace r (Requirements rl)

newtype Types f = Types [f] deriving (Eq, Show, Data, Typeable)
unTypes :: Types f -> [f]
unTypes (Types a) = a
class (Data a, Typeable f) => HasTypes f a | a -> f where
    getTypes :: a -> [f]
    getTypes = unTypes . fromJust . gfind
    setTypes :: [f] -> a -> a
    setTypes pl r = fromJust $ greplace r (Types pl)

newtype Constants f = Constants [f] deriving (Eq, Show, Data, Typeable)
unConstants :: Constants f -> [f]
unConstants (Constants a) = a
class (Data a, Typeable f) => HasConstants f a | a -> f where
    getConstants :: a -> [f]
    getConstants = unConstants . fromJust . gfind
    setConstants :: [f] -> a -> a
    setConstants pl r = fromJust $ greplace r (Constants pl)

newtype Initial f = Initial [f] deriving (Eq, Show, Data, Typeable)
unInitial :: Initial f -> [f]
unInitial (Initial a) = a
class (Data a, Typeable f) => HasInitial f a | a -> f where
    getInitial :: a -> [f]
    getInitial = unInitial . fromJust . gfind
    setInitial :: [f] -> a -> a
    setInitial pl r = fromJust $ greplace r (Initial pl)


newtype Predicates f = Predicates [f] deriving (Eq, Show, Data, Typeable)
unPredicates :: Predicates f -> [f]
unPredicates (Predicates a) = a
class (Data a, Typeable f) => HasPredicates f a | a -> f where
    getPredicates :: a -> [f]
    getPredicates = unPredicates . fromJust . gfind
    setPredicates :: [f] -> a -> a
    setPredicates pl r = fromJust $ greplace r (Predicates pl)

newtype Functions f = Functions [f] deriving (Eq, Show, Data, Typeable)
unFunctions :: Functions f -> [f]
unFunctions (Functions a) = a
class (Data a, Typeable f) => HasFunctions f a | a -> f where
    getFunctions :: a -> [f]
    getFunctions = unFunctions . fromJust . gfind
    setFunctions :: [f] -> a -> a
    setFunctions pl r = fromJust $ greplace r (Functions pl)

newtype Constraints f = Constraints [f] deriving (Eq, Show, Data, Typeable)
unConstraints :: Constraints f -> [f]
unConstraints (Constraints a) = a
class (Data a, Typeable f) => HasConstraints f a | a -> f where
    getConstraints :: a -> [f]
    getConstraints = unConstraints . fromJust . gfind
    setConstraints ::  [f] -> a -> a
    setConstraints pre r = fromJust $ greplace r (Constraints pre)

newtype Goal f = Goal (Maybe f) deriving (Eq, Show, Data, Typeable)
unGoal :: Goal f -> Maybe f
unGoal (Goal a) = a
class (Data a, Typeable f) => HasGoal f a | a -> f where
    getGoal :: a -> Maybe f
    getGoal = unGoal . fromJust . gfind
    setGoal :: Maybe f -> a -> a
    setGoal pre r = fromJust $ greplace r (Goal pre)

newtype Actions f = Actions [f] deriving (Eq, Show, Data, Typeable)
unActions :: Actions f -> [f]
unActions (Actions a) = a
class (Data a, Typeable f) => HasActions f a | a -> f where
    getActions :: a -> [f]
    getActions = unActions . fromJust . gfind
    setActions :: [f] -> a -> a
    setActions pl r = fromJust $ greplace r (Actions pl)

newtype Derived f = Derived [f] deriving (Eq, Show, Data, Typeable)
unDerived :: Derived f -> [f]
unDerived (Derived a) = a
class (Data a, Typeable f) => HasDerived f a | a -> f where
    getDerived :: a -> [f]
    getDerived = unDerived . fromJust . gfind
    setDerived :: [f] -> a -> a
    setDerived pl r = fromJust $ greplace r (Derived pl)

newtype Parameters f = Parameters [f] deriving (Eq, Show, Data, Typeable)
unParameters :: Parameters f -> [f]
unParameters (Parameters a) = a
class (Data a, Typeable f) => HasParameters f a | a -> f where
    getParameters :: a -> [f]
    getParameters = unParameters . fromJust . gfind
    setParameters :: [f] -> a -> a
    setParameters pl r = fromJust $ greplace r (Parameters pl)

newtype Precondition f = Precondition [f] deriving (Eq, Show, Data, Typeable)
unPrecondition :: Precondition f -> [f]
unPrecondition (Precondition a) = a
class (Data a, Typeable f) => HasPrecondition f a | a -> f where
    getPrecondition :: a -> [f]
    getPrecondition = unPrecondition . fromJust . gfind
    setPrecondition :: [f] -> a -> a
    setPrecondition pre r = fromJust $ greplace r (Precondition pre)

newtype Effect f = Effect [f] deriving (Eq, Show, Data, Typeable)
unEffect :: Effect f -> [f]
unEffect (Effect a) = a
class (Data a, Typeable f) => HasEffect f a | a -> f where
    getEffect :: a -> [f]
    getEffect = unEffect . fromJust . gfind
    setEffect :: [f] -> a -> a
    setEffect eff r = fromJust $ greplace r (Effect eff)
