{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE TypeOperators #-}  --- Needed for infix operator  in type constructor declaration
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE ScopedTypeVariables #-}


-- | Associated Type Families
module ATF where

import Data.Kind (Type)
import Data.Proxy
import GHC.TypeLits  -- SYMBOL, NAT
import Data.Monoid ((<>))

-- Poly kinds in both of parameter a and b
-- hetrogeneous linked list
data (a :: k1) :<< (b :: k2)
infixr 5 :<<

-- | Associated Type Family
  
-- ATF provide a way to bundle term level code with computed types
-- Type calss provide ad-hoc polymorphism
-- ASF alloow us to compute ad hoc types
  
class HasPrintf a where   -- type class HasPrintf
  type Printf a :: Type  -- associate type Printf 
  format :: String -> Proxy a -> Printf a  -- :: a :<< ... :<< "symbol"

  
-- | base Instance

-- Correspond to havinf no more parameter
-- We return our desired output type a - String here 
instance forall a. KnownSymbol a => HasPrintf (a :: Symbol) where
  type Printf a = String
  format s _ = s <> symbolVal (Proxy @a)


-- case where we want to insert additional text in our final formatted string
-- we do not have parameter to consume 
-- Hence we do not change resulting simpler type of Printf

-- # textInstance
instance (HasPrintf a, KnownSymbol text)
    => HasPrintf ((text :: Symbol) :<< a) where
  type Printf (text :<< a) = Printf a
  format s _ = format (s <> symbolVal (Proxy @text))
                      (Proxy @a)

-- # paramInstance
instance (HasPrintf a, Show param)
    => HasPrintf ((param :: Type) :<< a) where
  type Printf (param :<< a) = param -> Printf a
  format s _ param = format (s <> show param) (Proxy @a)

printf :: HasPrintf a => Proxy a -> Printf a
printf = format ""
  

-- # stringInstance using XFlexibleInstances
-- select this instance instead of 3rd when param is String
-- i.e param :<< a to String :<< a 
instance {-# OVERLAPPING #-} HasPrintf a
    => HasPrintf (String :<< a) where
  type Printf (String :<< a) = String -> Printf a
  format s _ param = format (s <> param) (Proxy @a)




func :: forall s t . KnownSymbol s => MyType s t -> String
func _ = symbolVal (Proxy :: Proxy s)

data MyType (s :: Symbol) t = MyType t
                            
