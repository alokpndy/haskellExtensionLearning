-- # pragmas
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies     #-}

module Kind where

-- # imports
import GHC.TypeLits


{-- Terms are value we can manipuate, the things exist at runtime
    Types are proofs to compiler and ourselves

    Analogous in type-level programming

    Types now get manipulated
    Kinds becomes proof   
--}

-- | TYPES are the kind of thing and also known as value type 
-- Maybe Int :: * is a TYPE but  Maybe :: * -> * is not 

-- | Higher kinded types -- which have type variables
-- Maybe :: TYPE -> TYPE   Either :: TYPE -> TYPE -> TYPE

-- CONSTRAINT is the kind of any fully-saturated typeclass
-- Show :: TYPE -> Constraint


-- | Data Kinds
-- -XDataKinds lifts data constructors into type constructors and types into kinds.

-- closed type families -- think as functions like AppendSymbol
-- | Using -XTypeFamilies we can promote regular function (term level function)
-- or writing our own

oor :: Bool -> Bool -> Bool
-- ^ term level function
oor True _ = True
oor False y = y 

type family Oor (x :: Bool) (y :: Bool) :: Bool where
-- ^ type family do not support currying    ^ return kind
  Oor 'True y = 'True
  Oor 'False y = y 

{-- Will not work as partial application is not supported
-- No currying i.e all argument must be supplied at a time
type family MMap (x :: a -> b) ( i :: [a]) :: [b] where
  MMap f '[] = '[]
  MMap f (y ': ys) = f y ': MMap f ys
--}  

-- | type family Bar x y :: Bool -> Bool -> Bool where
--                          -------------------- < return type a function 
-- Bar :: TYPE -> TYPE -> Bool -> Bool -> Bool


