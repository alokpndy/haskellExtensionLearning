{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE TypeOperators #-} -- used to perform arithmetic opetations on NAT
{-# LANGUAGE PolyKinds #-}

-- # pragmas
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies     #-}

-- # module
module Kind where

-- # imports
import GHC.TypeLits
import Data.Proxy (Proxy (..))
import Data.Kind

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


data UserType = User | Admin deriving Show
data User1 = User1 { userName :: Maybe (Proxy 'Admin) }  deriving Show
us = User1 (Just (Proxy )) 

type family DoSen (f :: *) :: Bool where
  DoSen (a -> b )  = True
  DoSen (a)  = False 


type family OneList (x :: [Nat]) :: Nat where 
  OneList '[] = 0
  OneList (x ': xs) = 1 + OneList xs


type family PrettyPrintList (vs :: [k]) :: ErrorMessage where
  PrettyPrintList '[]       = 'Text ""
  PrettyPrintList '[a]      = ShowTypeQuoted a
  PrettyPrintList '[a, b]   = ShowTypeQuoted a ':<>: 'Text ", and " ':<>: ShowTypeQuoted b
  PrettyPrintList (a ': vs) = ShowTypeQuoted a ':<>: 'Text ", " ':<>: PrettyPrintList vs

type family ShowTypeQuoted (t :: k) :: ErrorMessage where
  ShowTypeQuoted (t :: Symbol) = 'ShowType t
  ShowTypeQuoted t             = 'Text "'" ':<>: 'ShowType t ':<>: 'Text "'"

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

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True  y = 'True
  Or 'False y = y
  

type family Map (x :: a -> b) (i :: [a]) :: [b] where
  Map f '[]       = '[]
  Map f (x ': xs) = f x ': Map f xs

-- Mu if HIgher Order fixpoint operator 
data Mu f a = Roll (f (Mu f) a)  
data ListF f a = Nil | Cons a (f a)
type List a = Mu ListF a


type family ClosedComp (a :: Nat) (b:: Nat) :: Ordering where 
  ClosedComp x y = CmpNat x y
  --             ^ RHS of equation is funcction hence GHC cannot figure if
                 -- it will ever terminate, hence -XUndecidableInstances


type family Ops (x :: Bool) :: Bool where 
  Ops 'True = 'False
  Ops 'False = 'True 




type family Where (c :: k -> Constraint) (ts :: [Type]) :: Constraint where
  Where c '[] = ()
  Where c (t ': ts) = (c t, Where c ts)




class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  toNumber a = 100 

  {-# MINIMAL fromNumber #-}
  
newtype Age a =
  Age a deriving (Eq, Show)

instance (Integral a) => Numberish (Age a) where
  fromNumber n = Age (fromInteger n)
  toNumber (Age n) = (fromIntegral n)


