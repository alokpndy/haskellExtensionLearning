{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Exis2 where

import Data.Dynamic
import Data.Typeable
import Data.Kind
import Data.Kind (Type)
import GHC.Generics
import Data.Functor.Identity



-- | You loose your type forever, so do they
--   Existential types

data Any_ where
  Any_ :: (Typeable t, Show t)  => t ->Any_

instance Show Any_ where
  show (Any_ a) = "Just show " ++ show a

showType :: (forall a. Typeable a => a -> r) -> Any_ -> r 
showType f (Any_ a) = f a  







data Any' = forall a. Any' a
--                         ^ at right side only 
--                           hence Any' type const not know about a
-- a only exist in data const Any'

-- | writing above using GADTs -- Though a inside Any'' cannot be recovered unline HList
data Any'' where                             -- (1)
  Any'' :: a -> Any''
-- $  :t [Any'' 2, Any'' True] is [Any'']


-- * existential type can be eliminated by continuation passing.

-- an eliminator will be a Rank 2 function
elimAny'' :: (forall a. a -> r) -> Any'' -> r
--            Rank 2 hence, a depends upon calle while r on caller
elimAny'' f (Any'' a) = f a

-- packig a type calss dictionary along with existentialise data
data HasShow where
  HasShow :: Show t => t -> HasShow           -- (2)

-- we can write our own show Instance of HasShow or use the default
instance Show HasShow where
  show (HasShow a) = "HasSHow is " ++ show a
  
 -- $ map show [HasShow 2, HasShow True]

elimShow :: (forall a. Show a  => a -> r) -> HasShow -> r
elimShow f (HasShow a) = f a  








data HasShow2 where
  HasShow2 :: (Show t, Num t) => t -> HasShow2 

elimShow3 :: (forall a. Show a  => a -> r) -> HasShow2 -> r
elimShow3 f (HasShow2 a) = f a 

instance Show HasShow2 where
  show (HasShow2 a) = "HasSHow is " ++ show a

 


-- |GENERALISE CONSTRAIN KINDED EXISTENTIALS
-- -XConstraintKinds -- polymorphic over constraints
-- KindSignatures c is kind 
--             ------------------ must be fully saturated
data Has (c :: Type -> Constraint) where
  Has :: c t => t -> Has c 
  
elimHas :: (forall a. c a => a -> r) -> Has c -> r
elimHas f (Has c) = f c

type HasShow'' = Has Show
type HasDynamic = Has Typeable
-- type HasS = Has (Monoid, Eq)   -- wont work as type synonym must be fully saturated -- use constraint synonym
-- | constraint synonym
-- make a class with supercalss

class (Monoid a, Eq a) => MonoidEq a
-- with  {-# LANGUAGE FlexibleInstances #-}
instance (Monoid a, Eq a) => MonoidEq a


data Person = Person { name :: String
                              , age  :: Integer
                              } deriving (Generic)  -- using Derivegeneric
-- HKD -- Higher Kinded Data
type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a

-- f :: Type -> Type 
data Person' f = Person' { name' :: HKD f String
                         , age'  :: HKD f Integer
                         } deriving (Generic)  -- using Derivegeneric
-- :t name' @Identity is name' @Identity :: Person' Identity -> [Char]

valid :: Person' Maybe -> Maybe Person
valid (Person' n a) = Person <$> n <*> a



