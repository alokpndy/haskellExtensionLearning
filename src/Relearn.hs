
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Relearn () where

import Control.Monad 
import Control.Applicative 

-- |                          ----Lens 1----
data Lens s a = Lens
  { getter :: s -> a
  , setter :: a -> s -> s }


-- Apply   
setIth :: Int -> a -> [a] -> [a]
setIth _ _ [] = []
setIth i a (x:xs)
  | i == 0 =  (a : xs)
  | otherwise  = x : setIth (i-1) a xs 

ix :: Int -> Lens [a] a
ix i = Lens
  { getter = (!! i)
  , setter = setIth i
  }

-- |                         ----Lens 2----

-- return old value before modification
type Lens2 s a = (a -> a) -> s -> (a, s) 


setIth2 :: Int -> Lens2 [a] a
setIth2 index f (x:xs)
  | index == 0 = (x, f x : xs)
  | otherwise = second (x:) $ setIth2 (index -1) f xs 
    
second :: (b -> b) -> (a, b) -> (a,b)
second f (a,b) = (a, f  b) 

-- getter with id
-- setter with const


-- |                        ----Lens 3----
type Lens3 s a = forall m. Monad m => (a -> m a) -> s -> (a, m s)

-- list of list

setIth3 :: Int -> Lens3 [a] a
setIth3 index f (x:xs)
  | index == 0 = (x, liftM (: xs)  (f x))
  | otherwise = second (liftM (x:)) $ setIth3 (index -1) f xs 
    


-- |                        ----Lens 4----

type Lens4 s a = (forall f. (Functor f) => (a -> f a) -> s -> (a, f s))

setIth4 :: Int -> Lens4 [a] a
setIth4 index f (x:xs)
  | index == 0 = (x, (: xs) <$> f x)
  | otherwise = second ((x:) <$>) $ setIth4 (index -1) f xs 



-- |                      ----Lens 5-----
type Lens5 s a = forall f. Functor f => (a -> f a) -> s -> f s

setIth5 :: Int -> Lens5 [a] a
setIth5 index f (x:xs)
  | index == 0 =  (: xs) <$> f x
  | otherwise = (x :) <$> setIth5 (index - 1) f xs
  
data Storey x f a = Storey x (f a)
  deriving Show

instance Functor f => Functor (Storey x f) where
  fmap f (Storey x fa) = Storey x (fmap f fa)
