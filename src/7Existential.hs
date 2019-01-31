
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}


module Existential where

import Data.Dynamic
import Data.Typeable
import Data.Monoid 
import Data.Coerce
import Data.IORef
import System.IO.Unsafe

data Any1 = forall a. Any1 a
-- ^ ANy can store any type but we type constructor cannot tell
-- which type as it does not have any type variable
-- Now a only exixt within context of data constructor Any

-- using GADTs to write Existentialquantification
data Any_ where
  Any_ :: a -> Any_
  
-- [Any 4, Any True, Any "hello"] :: [Any]

eliminateAny :: (forall a. a -> r) -> Any1 -> r
eliminateAny f (Any1 a) = f a


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





slower :: [Int] -> Int
slower  = getSum . mconcat . fmap Sum


faster :: [Int] -> Int
faster = getSum . mconcat . coerce




first :: (Enum a, Enum b) =>(forall x. Enum x => x -> x) -> a -> b -> (a,b)
first f x y  = (f x, f y)

data Many = forall a. Many a

data MAny_ where
  MAny_ :: Show a =>  a -> MAny_
  
elim ::  (forall a. Show a => a -> r) -> MAny_ -> r
elim f (MAny_ a ) = f a

instance Show MAny_ where
  show (MAny_ a ) = show a

data Runner where
  Runner :: Typeable a => a -> Runner
  
elimRunner :: (forall a. Typeable a => a -> r) -> Runner ->  r
elimRunner f (Runner a) = f a 

fromRunner :: Typeable a => Runner -> Maybe a
fromRunner = elimRunner cast




-- | ST TRICK
newtype ST s a = ST { unsafeRunST :: a }

instance Functor (ST s) where
  fmap f (ST a) = seq a . ST $ f a

instance Applicative (ST s) where
  pure = ST
  ST f <*> ST a = seq f . seq a . ST $ f a

instance Monad (ST s) where
  ST a >>= f = seq a $ f a

newtype STRef s a = STRef { unSTRef :: IORef a }

newSTRef :: a -> ST s (STRef s a)
newSTRef = pure . STRef .  unsafePerformIO  . newIORef


