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
import Control.Monad (join)



-- Poly kinds in both of parameter a and b
data (a :: k1) :<< (b :: k2)
infixr 5 :<<

-- Combine two :<< types 
data (a :: k1) :<|> (b :: k2)
infixr 5 :<|>


  
  
-- Associated Type Class 
class HasPrintf a where
  type Printf a :: Type -- Associate Type
  format :: String -> Proxy a -> Printf a 

-- # base instance
-- as our schema "... :<< "symbol" " alwasy end with SYMBOL, make it our base case
-- at end of Schema this instance will be applied
-- no type level recursion to be done hence just return our desired outout type String 
instance forall a. KnownSymbol a => HasPrintf (a :: Symbol) where
  type Printf a = String
  format s _ = s  <> symbolVal (Proxy @a) -- Proxy :: Proxy a -- wriiten as Proxy @a using -XTypeApplications
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



-- # stringInstance with String type parameter 
instance {-# OVERLAPPING #-} HasPrintf a
    => HasPrintf (String :<< a) where
  type Printf (String :<< a) = String -> Printf a
  format s _ param = format (s <> param) (Proxy @a)


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




result = show $ toConstr (User "a" 2 (Just 2))











class HasSchema a where
  type Schema a :: Type
  change :: String -> Proxy a -> Schema a

instance KnownSymbol a => HasSchema (a :: Symbol ) where
  type Schema a = String
  change s _ = s <> symbolVal (Proxy @a) 
  
instance (HasSchema a, KnownSymbol s) => HasSchema ((s :: Symbol) :<< a) where
  type Schema (s :<< a) = Schema a
  change s _ = change (s <> symbolVal (Proxy @s)) (Proxy @a)

instance (HasSchema a, Show p) => HasSchema ((p :: Type) :<< a) where
  type Schema (p :<< a) = p -> Schema a
  change s _ p = change (s <> show p) (Proxy @a)

hasf :: HasSchema a => Proxy a -> Schema a 
hasf = change "" 

instance (HasSchema a, KnownNat s) => HasSchema ((s :: Nat) :<< a) where
  type Schema (s :<< a) = Schema a
  change s _ = change (s <> (show . natVal) (Proxy @s)) (Proxy @a) 
