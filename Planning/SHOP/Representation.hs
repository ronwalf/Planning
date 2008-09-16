module Planning.SHOP.Representation (
    module Planning.Expressions,
    module Planning.Records,

    Domain(..),
    
    Method(..),

    Operator(..),

    Problem(..),

    SHOPDoc(..),
    shopExprDoc
) where

import Data.Generics (Data, Typeable, Typeable2)
import Data.List
import Text.PrettyPrint

import Planning.Expressions
import Planning.Records


data List e = List [e] deriving (Data, Eq)
deriving instance Typeable1 List
instance Functor List where
    fmap f (List l) = List $ map f l
instance FuncEq List where
    funcEq (List l1) (List l2) = (l1 == l2)
eList l = inject (List l)


