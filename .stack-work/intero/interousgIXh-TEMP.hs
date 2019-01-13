
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}


module Existential where

import Data.Dynamic
import Data.Typeable

data Any = forall a. Any a
-- ^ ANy can store any type but we type constructor cannot tell
-- which type as it does not have any type variable
-- Now a only exixt within context of data constructor Any

-- using GADTs to write Existentialquantification
data Any_ where
  Any_ :: a -> Any_
  
-- [Any 4, Any True, Any "hello"] :: [Any]

eliminateAny :: (forall a. a -> r) -> Any -> r
eliminateAny f (Any a) = f a


data HasShow where
  HasShow :: Show t => t -> HasShow

instance Show HasShow where
  show (HasShow s) = "HasShow " ++ show s
  
eliminateHasShow :: (forall a. Show a => a -> r) -> HasShow -> r
eliminateHasShow f (HasShow a) = f a

-- eliminatehasshow show (Hasshow "OK")

-- Typeable from Data.Dynamic
data Tynamic where
  Tynamic :: Typeable t => t -> Tynamic
  
elimelimDynamic
    :: (forall a. Typeable a => a -> r)
    -> Tynamic
    -> r
elimelimDynamic f (Tynamic a) = f a
