
-- https://blog.jle.im/entry/introduction-to-singletons-1.html

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}


module Singleton1 where

import Data.Kind

-- Phantoms Type
data DoorState = Opened | Closed | Locked
  deriving (Eq, Show)
-- data Door (s :: DoorState) = UnsafeMkDoor { doorMaterial :: String }

data Door :: DoorState -> Type where
  UnsafeMkDoor :: forall s.  {doorMaterial :: String } -> Door s
  --                                                       ^called indexed data type
  -- s will be decided by caller
  
closeDoor :: Door 'Opened -> Door 'Closed
closeDoor (UnsafeMkDoor m) = UnsafeMkDoor m 

lockDoor :: Door 'Closed -> Door 'Locked
lockDoor (UnsafeMkDoor m) = UnsafeMkDoor m

openDoor :: Door 'Closed -> Door 'Opened
openDoor (UnsafeMkDoor m) = UnsafeMkDoor m

-- ____ PHANTOM MENACE ______
-- How can we get Door State from Door



-- | Singleton in Haskell is a Type that has exactly one inhabitant
data SingDS :: DoorState -> Type where
  --  dependent pattern match
  SOpened :: SingDS 'Opened
  SClosed :: SingDS 'Closed
  SLocked :: SingDS 'Locked
  -- ^ this are witness od s and exist at runtime contra to phantom
  -- hence we can patter match on these value
  -- Types are themselves erased but these bypass TypeErasure
  

lockAnyDoor :: SingDS s -> Door s -> Door 'Locked
lockAnyDoor sng door = case sng of
    SOpened -> lockDoor (closeDoor door) -- in this branch, s is 'Opened
    SClosed -> lockDoor door             -- in this branch, s is 'Closed
    SLocked -> door                      -- in this branch, s is 'Locked


-- | Reflection

-- the process of turning type variable like 's' into dynamic runtime value
-- i.e we can move a value from type level to term level 
-- we can exclude Door s as is it not necessary 
doorStatus :: SingDS s -> Door s -> DoorState
doorStatus SOpened _ = Opened
doorStatus SClosed _ = Closed
doorStatus SLocked _ = Locked


-- | Recoverig Implicit Passing

-- ie automatically pass witness
class SingDSI s where
  singDS :: SingDS s

instance SingDSI 'Opened where
  singDS = SOpened
instance SingDSI 'Closed where
  singDS = SClosed
instance SingDSI 'Locked where
  singDS = SLocked

-- Now
lockAnyDoor_ :: SingDSI s => Door s -> Door 'Locked
lockAnyDoor_ = lockAnyDoor singDS


-- $  :t lockAnyDoor_ (UnsafeMkDoor @ 'Opened "Wood")
