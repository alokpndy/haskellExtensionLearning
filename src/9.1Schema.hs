{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-} -- For Overlapping Instances
{-# LANGUAGE TypeOperators #-}  --- Needed for infix operator  in type constructor declaration
{-# LANGUAGE KindSignatures #-} -- Sigature of type kinds
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}  -- fmap @Maybe
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-} -- for makeing a scope availabe to whole function
{-# LANGUAGE DataKinds #-}  -- promotion of types, data 

-- # module name
module Schema where

-- # imports list
import Data.Monoid ( (<>) )
import Data.Proxy (Proxy (..))
import GHC.TypeLits (Symbol, symbolVal, KnownSymbol, someSymbolVal, Nat, KnownNat, natVal)  -- Nat, class KnownSymbols, symbolVal, natVal, etc 
import Data.Kind (Type)

import Data.Data
import Data.String -- better use other 



-- Poly kinds in both of parameter a and b
data (a :: k1) :<< (b :: k2)
infixr 5 :<<


-- Associated Type Class 
class HasPrintf a where
  type Printf a :: Type -- Associate Type
  format :: String -> Proxy a -> Printf a 

-- # base instance
-- no parameter; just append final string to desired types i.e String in thsi case
instance forall a. KnownSymbol a => HasPrintf (a :: Symbol) where
  type Printf a = String
  format s _ = s  <> symbolVal (Proxy @a) -- Proxy :: Proxy a -- wriiten as Proxy @a using -XTpeApplicatons
--                    ^ will need KnownSymbol class constraint

-- # text Instance
-- additional text to be injected to final string; No params hence Printf type remain same
instance (HasPrintf a, KnownSymbol text) => HasPrintf ((text :: Symbol) :<< a) where
  type Printf (text :<< a)  = Printf a
  format s _ = format (s  <> symbolVal (Proxy @text)) (Proxy @a)

-- # parameter Instance
-- return Printf type and can continue later -- end it by case 1 i.e supplying string
instance (HasPrintf a, Show param) => HasPrintf ((param :: Type) :<< a) where
  type Printf (param :<< a) = param -> Printf a
  format s _ param = format (s  <>  show param) (Proxy @a) 




printf :: HasPrintf a => Proxy a -> Printf a 
printf = format "" 


-- Nat instance for natural number --
-- weill fail for fraction etc
instance (HasPrintf a, KnownNat num) => HasPrintf ((num :: Nat) :<< a) where
  type Printf (num :<< a)  = Printf a
  format s _ = format (s  <>  ( (show . natVal) (Proxy @num)) ) (Proxy @a)



data User = User { name :: String
                 , age  :: Integer
                 , id   :: Maybe Integer
                 } deriving (Show, Eq, Data, Typeable)


data D = X Int | Y Int Int
  deriving (Data,Typeable)

result = show $ toConstr (X 3)
