{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE TypeApplications #-}
-- # pragma 2
{-# LANGUAGE TypeFamilies #-}

module FirstClassFamilies where

import Data.Kind
import GHC.TypeLits (AppendSymbol, Symbol, symbolVal)
import Data.Proxy



data Fst a b = Fst (a, b)

class EvalC l t | l -> t where
  evalC :: l -> t

instance EvalC (Fst a b) a where
  evalC (Fst (a,b)) = a 


data MapList dfb a = MapList (a -> dfb) [a]

instance EvalC dfb dft => EvalC (MapList dfb a) [dft] where
  evalC (MapList f []) = []
  evalC (MapList f (a:as)) = evalC (f a) : evalC (MapList f as) 


type Exp a = a -> Type

type family Eval (e :: Exp a) :: a

data Snd :: (a,b) -> Exp b

type instance Eval (Snd '( a, b)) = b


data FromMaybe :: a -> Maybe a -> Exp a

data ListToMaybe :: [a] -> Exp (Maybe a)

type instance Eval (ListToMaybe '[]) = 'Nothing
type instance Eval (ListToMaybe (a ': _)) = 'Just a



data MapList' :: (a -> Exp b) -> [a] -> Exp [b]

type instance Eval (MapList' f '[]) = '[]

type instance Eval (MapList' f (a ': as)) = Eval (f a) ': Eval (MapList' f as) 



data FoldR :: (a -> b -> Exp b) -> b -> [a] -> Exp b

type instance Eval (FoldR f b '[]) = '[]

type instance Eval (FoldR f b (a ': as) ) = Eval (a `f` (Eval (FoldR f b  as)) )




data EventType = WakeUp | Eat Meal
           
type Meal = String

data PayloadWakeUp = PayloadWakeUp'
data PayloadEat = PayloadEat' Meal

data Event = EventWakeUp PayloadWakeUp | EventEat PayloadEat

data family Payload (e :: EventType)  
data instance Payload 'WakeUp =  PayloadWakeUp

