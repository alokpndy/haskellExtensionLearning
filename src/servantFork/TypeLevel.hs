{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleInstances #-} --  instance head is no longer just a single type constructor and type variables.
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}


module TypeLevel where

import Data.Kind
import GHC.TypeLits 
import Data.Proxy
import Data.Typeable
import Data.Functor
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Data.Functor.Identity


-- | data type declaration 
data (path :: k) :> (b :: k2)
  deriving (Typeable) 
infixr 5 :>

data a :<|> b = a :<|> b
    deriving (Typeable, Eq, Show, Functor, Traversable, Foldable, Bounded)
infixr 3 :<|>

instance (Semigroup a, Semigroup b) => Semigroup (a :<|> b) where
    (a :<|> b) <> (a' :<|> b') = (a <> a') :<|> (b <> b')

instance (Monoid a, Monoid b) => Monoid (a :<|> b) where
    mempty = mempty :<|> mempty
    (a :<|> b) `mappend` (a' :<|> b') = (a `mappend` a') :<|> (b `mappend` b')
    


-- | Endpoints 
type family Endpoints api where
  Endpoints (a :<|> b) = AppendList (Endpoints a) (Endpoints b)
  Endpoints (e :> a)   = MapSub e (Endpoints a)
  Endpoints  a         = '[a]

--  Check if endpoint is withinn api 
type family IsElem' a s :: Constraint

type family IsElem endpoint api :: Constraint where
  IsElem e (sa :<|> sb)         =  Or (IsElem e sa) (IsElem e sb)
  IsElem (e :> sa) (e :> sb)    =  IsElem sa sb
  IsElem (Capture z y :> sa) (Capture x y :> sb)
                                          = IsElem sa sb                                       
  IsElem e e                              = ()
  IsElem e a                              = IsElem' e a


-- | Utils
type Capture = Capture' '[]

data Capture' (mods :: [Type]) (sym :: Symbol) (a :: Type)
    deriving (Typeable)

data CaptureAll (sym :: Symbol) (a :: Type)
    deriving (Typeable)

type family MapSub e xs where
  MapSub e '[] = '[]
  MapSub e (x ': xs) = (e :> x) ': MapSub e xs

type family AppendList xs ys where
  AppendList '[]       ys = ys
  AppendList (x ': xs) ys = x ': AppendList xs ys

type family Or (a :: Constraint) (b :: Constraint) :: Constraint where
  Or () b       = ()
  Or a ()       = ()

type family And (a :: Constraint) (b :: Constraint) :: Constraint where
  And () ()     = ()
                                          
-- to skip custom things like QueryParam from link
data CustomThing
type instance IsElem' e (CustomThing :> s) = IsElem e s

-- Exercise 
type SampleApi = "hello" :> Capture "name" String
               :<|> "bye" :> Capture "greet" String
type FstApi =  "hello" :> Capture "name" String

type Api2  = "hello" :> Capture "name" String
               :<|> Bool :> Capture "greet" String

data Link = Link
  { _segments    :: [String]
  } deriving Show


safeLink'
    :: forall endpoint  a. (IsElem endpoint endpoint, HasLinks endpoint)
    => Proxy endpoint       -- ^ The whole API that this endpoint is a part of
    -> Proxy endpoint -- ^ The API endpoint you would like to point to
    -> MkLink endpoint   
safeLink' ap endpoint = format "" endpoint   

type A = "Ap" :> Int
type AB = "Ap" :> Int :<|> "do" :> Int 



-- Associated Type Class 
class HasLinks a where
  type MkLink a :: Type -- Associate Type
  format :: String -> Proxy a -> MkLink a 

instance forall a. KnownSymbol a => HasLinks (a :: Symbol) where
  type MkLink a = String
  format s _ = s  <> symbolVal (Proxy @a)

instance (HasLinks a, KnownSymbol text) => HasLinks ((text :: Symbol) :> a) where
  type MkLink (text :> a)  = MkLink a
  format s _ = format (s  <> symbolVal (Proxy @text)) (Proxy @a)

 
instance (HasLinks a, Show param) => HasLinks ((param :: Type) :> a) where
  type MkLink (param :> a) = param -> MkLink a
  format s _ param = format (s  <>  show param) (Proxy @a)

{-
instance {-# OVERLAPPING #-} (HasLinks a, Show b) => HasLinks (CaptureAll sym b :> a) where
  type MkLink (CaptureAll sym b :> a) = b -> MkLink a
  format s _ b = format (s  <>  show b) (Proxy @a)
-}

-- $ safeLink' (Proxy @(CaptureAll  "name" String :> "m")) (Proxy @(CaptureAll  "name" String :> "m")) " po "
-- $  =  "\" po \"m"

instance {-# OVERLAPPING #-} HasLinks a
    => HasLinks (String :> a) where
  type MkLink (String :> a) = String -> MkLink a
  format s _ param = format (s <> param) (Proxy @a)


instance forall a. KnownNat a => HasLinks (a :: Nat) where
  type MkLink a = Integer
  format s _  =  1 + natVal (Proxy @a)

data Key = Key { a :: Integer } deriving (Show, Eq) 

type family Bound f a :: Type where
  Bound Identity a = a
  Bound f a =  f a

inBound :: Key -> Maybe Key
inBound (Key a) = if a >= 5 && a <= 15 then Just (Key a)
  else Nothing


 
