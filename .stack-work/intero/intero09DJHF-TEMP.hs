{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications  #-}

{-# LANGUAGE RankNTypes #-}

module RankN where

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


cont :: a -> (forall r. (a -> r) -> r)
cont a = \callback -> callback a

first :: (Enum a, Enum b) =>(forall x. Enum x => x -> x) -> a -> b -> (a,b)
first f x y  = (f x, f y) 
