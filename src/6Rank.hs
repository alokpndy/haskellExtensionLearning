{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications  #-}

{-# LANGUAGE RankNTypes #-}

module RankN where

import Data.Functor
import Control.Applicative
import Control.Monad 

-- | Endomorphism - The functions which takes and return the same type
-- eg not :: Bool -> Bool, show @String :: String -> String

id_ :: forall a. a -> a
id_ a = a 

applyToFive :: (forall a. a -> a) -> Int
-- ^ Using RankNTypes we seperate forall inside bracket
applyToFive f = f 5
-- applytoFive = id_

-- | Rank
--  rank of a function is depth of its polymorphism


-- forall a. a -> a    is better written as forall a. (a -> a)
foo :: forall r. ((forall a. Show a =>  a -> r) -> r)
-- r is Rank 0 hence must be instantiated by caller
-- implementation of foo (Callee)  get to declare 'a' as it of Rank 1 -- here it is Int as
-- f 2    where f :: a -> r  or f :: Int -> r   as we habe give 2 as a
foo f = f 2 
-- And so, the rank of a function is simply the
-- number of arrows its deepest forall is to the left of.




-- | Continuation Passing Style CPS
-- The type  forall r. (a  -> r) -> r is know to be in continuation passing style 
cont :: a ->  forall r. (a  -> r) -> r
cont  a = \callback -> callback a
--        \ ( a -> r ) a
--        \ r -> r

runCont :: (forall r. (a -> r) -> r) -> a
runCont f = f id  

newtype Cont a = Cont { unCont :: forall r. (a -> r) -> r }

instance Functor Cont where 
  fmap f (Cont ca)  = Cont $  \r -> (ca (r . f))

instance Applicative Cont where
  pure a = Cont $ \c -> c a
  Cont f <*> Cont a = Cont $ \br -> f $ \ab -> a $ br . ab


instance Monad Cont where
  return = pure
  Cont m >>= f = Cont $ \c -> m $ \a -> unCont (f a) c


doLater :: String 
doLater  = runCont $  unCont $ do 
  d <- Cont dob
  s <- Cont so
  pure $ (show d) ++ (show s) 

 
dob ::  forall r.  (Int -> r) -> r
dob f = f 6 
so ::  forall r.  (Double -> r) -> r
so f = f 1.0


