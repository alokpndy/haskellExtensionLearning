{-# LANGUAGE TypeInType #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module QuickTest where

import Data.Kind

-- type function or indexed type family
data Nat = Zero | Succ Nat

type family Foo (a:: Nat) (b :: Nat) :: Nat
type instance Foo 'Zero b = b
type instance Foo ('Succ a) b = 'Succ (Foo a b)  



-- paramterised datatypes
data HList :: [Type] -> Type where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as) 

data EqR :: forall x.  (x -> Type) -> x -> Type where
  EqA :: (Int -> Bool) -> Int -> EqR f Bool 

eval :: EqR f x -> x
eval (EqA f x ) = f 1

data TApp f a = MkTApp (f a)
