{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS -Wall #-}

module ConstraintsAndGADTs where

import Data.Kind (Constraint, Type)
import GHC.TypeLits



-- | Type CONSTRAINTS are reserved for left of =>
-- Type Equalities   (a ~ Int ) enabledby GADTs

five :: Int
five = 5
-- Type equalities
five_ :: (a ~ Int) => a
-- ^ Identical as five  
five_ = 5

{-- Type equalities form an equivalence relation, meaning
 -- reflexivity—a type is always equal to itself: a ∼ a
 -- symmetry— a∼b holds if and only if b ∼a
 -- transitivity—if we know both a ∼ b and b ∼ c,
    we (and GHC) can infer that a ∼ c.
--}

data Math a where
  Nt :: Integer -> Math Integer
  Plus :: Math Integer -> Math Integer -> Math Integer

eval1 :: Math  a -> a
eval1 (Nt n)       = n
eval1 (Plus e1 e2) = eval1 e1 + eval1 e2

-- Generalised Algebraic Data Type
data Exp a where
    I :: Int -> Exp Int
    B :: Bool -> Exp Bool
    Add :: Exp Int -> Exp Int -> Exp Int
    Mul :: Exp Int -> Exp Int -> Exp Int
    Equ :: Exp Int -> Exp Int -> Exp Bool

eval :: Exp a -> a
eval (I n)       = n
eval (B b)       = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Equ e1 e2) = eval e1 ==  eval e2

-- | GADTs are merely syntactic sugar over type equalities
data Exp_ a =
   (a ~ Integer) => A a
 | (a ~ Bool) => X a
 | (a ~ Integer) => Add_ (Exp_ a) (Exp_ a)
 | (a ~ Bool) => Equ_ (Exp_ a) (Exp_ a) 

eval_ :: Exp_ a -> a
eval_ (A y) = y
eval_ (X z) = z
eval_ (Add_ x1 x2) = eval_ x1 + eval_ x2
eval_ (Equ_ y1 y2) = eval_ y1 == eval_ y2


-- | Hetrogeneous List

type family AllEq (ts :: [Type]) :: Constraint where
  AllEq '[] = ()
  AllEq (x ': xs) = (Eq x,  AllEq xs)

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)
infixr 5 :#

hLength :: HList ts -> Int
hLength HNil = 0
hLength (_ :# ts) = 1 + hLength ts


headH :: HList (t ': ts) -> t
headH (t :# _) = t 
-- headH $ (:#) (Just "Alok") (HNil)

-- using constraintkinds
type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts) 
-- Now we can write
instance All Eq ts => Eq (HList ts) where
  HNil == HNil = True
  (a :# as) == (b :# bs) = a == b && as == bs

hls = (:#) 1 :# Nothing :# HNil

  


data Reval  a where
  PInt :: Int -> Reval Int
  PBool :: Bool -> Reval Bool
  Radd :: Reval Int ->  Reval Int ->  Reval Int
  Rot ::  Reval Bool -> Reval Bool

eReval :: Reval a -> a
eReval (PInt i) = i
eReval (PBool b) = b
eReval (Radd i j) = eReval i + eReval j
eReval (Rot a) = not $ eReval a 



-- Hetrogeneous List

data MList (ts :: [Type]) where
  MNil :: MList '[]
  (:*) :: t -> MList ts -> MList (t ': ts)
infixr 5 :*


mHead :: MList (t ': ts) -> t
mHead (t :* _) = t

mLength :: MList ts -> Int
mLength MNil = 0
mLength (_ :* ts) = 1 + mLength ts 

{-
instance Eq (MList '[]) where
  MNil == MNil = True

instance (Eq t, Eq (MList ts)) => Eq (MList (t ': ts)) where
  (a :* as) == (b :* bs) = a == b && as == bs 

-}
instance (Show t, Show (MList ts)) => Show (MList (t ': ts)) where
  show (a :* as)  = show a <> " :* " <> show as 
instance  Show (MList '[]) where
  show MNil = "MNil"


type family EachEq (ts :: [Type]) :: Constraint where
  EachEq '[] = ()
  EachEq (x ': xs) = (Eq x, EachEq xs)


type family EachEq' (x :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  EachEq' c  '[] = ()
  EachEq' c (x ': xs) = (c x, EachEq' c  xs)

instance EachEq' Eq ts => Eq (MList ts) where
  MNil == MNil = True
  (a :* as) == (b :* bs) = a == b && as == bs 







