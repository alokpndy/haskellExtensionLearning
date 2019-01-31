{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- # pragma 2
{-# LANGUAGE TypeFamilies #-}

module FirstClassFamilies where

import Data.Kind

--  making type families first class
-- | Defunctionalisation

-- the process of replacing an instantiation of a polymorphic -
-- function with a specialized label instead.

{- fst :: (a,b) -> a
   fst (a,b) = a
-}
-- defunctionalised fst
data Fst a b = Fst (a,b)

class Eval l t | l -> t where
  -- multiparamtypeclasses   as l and t are multiple constraint
  -- functionaldependencies says 't' is fully determined by 'l'
  eval :: l -> t
  
instance Eval (Fst a b) a where
  -- Flexibleinstances to allow (Fst a b) a 
  eval (Fst (a,b)) = a

data MapLst dfb a = MapLst (a -> dfb) [a]
instance Eval dfb dft => Eval (MapLst dfb a) [dft] where
  eval (MapLst f []) = []
  eval (MapLst f (x:xs)) = eval (f x) : eval (MapLst f xs) 
-- $ eval (MapLst Fst [(1,2)])


-- | 2 Type level defunctionalisation
-- First class family

type Exp a = a -> Type -- kind synonym

-- Open type family
type family Eval2 (e :: Exp a) :: a
-- Data Kind made Exy p type,  a is also type hence TypeInType

-- writing defunctionalised label use empty data types
data Snd :: (a,b) -> Exp b

type instance Eval2 (Snd '(a,b)) = b 
-- $ :kind! Eval2 (Snd '(1,2))
-- $ Eval2 (Snd '(1,2)) :: ghc-prim-0.5.2.0:GHC.Types.Nat


data MapLst2 :: (a -> Exp b) -> [a] -> Exp [b]
type instance Eval2 (MapLst2 f '[]) = '[]
type instance Eval2 (MapLst2 f (x ': xs)) = Eval2 (f x) ': Eval2 (MapLst2 f xs)
-- $ :kind! Eval2 ( MapLst2 Snd '[ '(1, 2) ] )
-- $ Eval2 ( MapLst2 Snd '[ '(1, 2) ] ) :: [ghc-prim-0.5.2.0:GHC.Types.Nat]

-- # pure 
data Pure :: a -> Exp a
type instance Eval2 (Pure a) = a
-- # bind
data (=<<) :: (a -> Exp b) -> Exp a -> Exp b
-- # Kleisli
data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c

type Snd2 = Snd <=< Snd
-- $ Snd <=< Snd :: (a1, (a2, c)) -> c -> * 
