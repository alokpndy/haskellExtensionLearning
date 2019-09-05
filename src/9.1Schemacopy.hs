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



data (a :: k1) :<|> (b :: k2)
infixr 5 :<|>

-- type family
class HasPrintf a where
  type Printf a :: Type -- associated type
  format :: String -> Proxy a -> Printf a

instance KnownSymbol text =>  HasPrintf (text :: Symbol) where   -- text :: Kind not Type hence XPolyKinds
  type Printf text = String
  format s _ = s <> symbolVal (Proxy @text) -- XScoopedType and XTypeApplication

instance (HasPrintf a, KnownSymbol txt) => HasPrintf ((txt :: Symbol) :<|> a) where
  type Printf (txt :<|> a) = Printf a
  format s _ = format (s <> symbolVal (Proxy @txt)) (Proxy @a)

instance (HasPrintf a, Show param) => HasPrintf ((param :: Type) :<|> a) where
  type Printf ((param :: Type) :<|> a) = param -> Printf a
  format s _ param = format (s <> show param) (Proxy @a)

instance (HasPrintf a, KnownNat num) => HasPrintf ((num :: Nat) :<|> a) where
  type Printf (num :<|> a) = Printf a
  format s _ = format (s <> show (natVal (Proxy @num))) (Proxy @a) 

printf :: HasPrintf a => Proxy a -> Printf a
printf = format "" 
