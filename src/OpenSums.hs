{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module OpenSums where

import Data.Kind

--Open Sums
-- Using -XPolyKinds, polyorphic k cna be (* -> *) or * or (* -> * -> *) etc
-- f :: ((* -> *) -> *) or just (* -> *) 
data OpenSum (f :: k -> Type) (ts :: [k]) where
  UnsafeOpenSum :: Int -> f t -> OpenSum f ts
  --                        ^ is not mentioned in return type hence it is a exixtential
  --                          but t must be element of ts :: [t]
  -- Int will be used for tag t and index ts
