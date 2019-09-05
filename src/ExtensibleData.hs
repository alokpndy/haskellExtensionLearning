{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}  -- Use to restrict polymorphism by providing kind signature 
-- {-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}

module ExtensibleData where 

import Data.Typeable
import GHC.TypeLits
import Data.Dynamic 
import Data.Maybe 
import Data.Foldable
import Data.Kind  
import Fcf.Data.List
import Fcf.Utils
import Fcf.Data.Common
import Fcf.Combinators

-- | Open Sum Type

-- container whose type is not known statically
-- its type might polymorphic

data Change where
  Change :: Typeable t => t -> Change

elimChange :: (forall a. Typeable a => a -> r) -> Change -> r
elimChange f (Change a) = f a 

fromChange :: Typeable a => Change -> Maybe a
fromChange = elimChange cast 


liftD2 :: forall a b r. (Typeable a, Typeable b, Typeable r) => Change -> Change -> (a -> b -> r) -> Maybe Change
liftD2 d1 d2 f = fmap Change . f <$> fromChange @a d1 <*> fromChange @b d2 


myPlus :: Change -> Change -> Change
myPlus a b =
  fromMaybe (error "bad types for myPlus") $ asum
     [ liftD2 @String @String a b (++)
     , liftD2 @Int @Int a b (+)
     , liftD2 @String @Int a b $ \strA intB -> strA ++ show intB
     , liftD2 @Int @String a b $ \intA strB -> show intA ++ strB
     ]


-- fromChange @String (myPlus (Change "1") (Change "2"))
-- fromChange @Int (myPlus (Change 1) (Change 2))



data Has (c :: Type -> Constraint) where
  Has :: c t => t -> Has c  
  
elimHas :: (forall a. c a => a -> r) -> Has c -> r
elimHas f (Has a) = f a

type HasShow  = Has Show

-- | Multiple COnstraints at  -- class synonym

class (Show a, Eq a) => Shoq a where {}
instance (Show a, Eq a) => Shoq a where {}  -- using Flexibleinstances -- also Undecidableinstances (as Constraint has more a tha head)




data OpenSum (f :: k -> Type) (ts :: [k]) where
  UnsafeOpenSum :: Int -> f t -> OpenSum f ts 

type FindElem (key :: k) (ts :: [k]) =
  FromMaybe Stuck =<< FindIndex (TyEq key) ts  


applyf :: (forall r. (a -> r) -> r ) -> a
applyf f = f id

unapplyf :: a -> (forall r. (a -> r) -> r )
unapplyf a = \callback -> callback a

newtype Cont a = Cont { runCont :: (forall r. (a -> r) -> r)  }




newtype Ex s a = Ex { runEx :: a }

newtype ExRef s a = ExRef { runExRef :: a }

makeExRef :: a -> Ex s (ExRef s a)
makeExRef = \x -> Ex $ ExRef x

runE :: (forall s. Ex s a) -> a
runE = runEx



data Mu f a = Ro (f (Mu f) a) 
