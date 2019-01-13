module Algebra where

import GHC.Word


-- | Cardinality can be writen as
data Void
-- ^ |Void| = 0
data Cool = Yes | No
-- ^ |Cool| = 2
{--
  Any two types that have the same cardinality will always be
  isomorphic to one another.

  to :: s -> t
  from :: t -> s

  to . form = id
  from . to = id
􏰚
--}

-- | Spin is isomorphic to Bool with cardinality of 2
data Spin = Up | Down

{--
  In general, for any two types with cardinality n,
  there are n! unique isomorphisms between them.
--}


-- | Sum, Product and Exponential types
data Deaal a b = This a
                 | That b
                 | TheOther Bool
-- ^ |Deal a b| = |a| + |b| + |Bool|
--                |a| + |b| + 2

data MixedFraction a = Fraction
  { mixedBit :: Word8
  , numerator :: a
  , denominator :: a
  }
-- ^ |MixedFraction a| = |Word8| x |a| x |a|
--                     = 256     x |a| x |a|



data TicTacToe a = TicTacToe
  { topLeft :: a
  , topCenter :: a
  , topRight :: a
  , midLeft :: a
  , midCenter :: a
  , midRIght :: a
  , botLeft :: a
  , botCenter :: a
  , botRight ::a
  }
-- ^ |Tictactoe a| = |a| x |a| x .. x |a|
--                 = |a|^9
--                 = |a|^ 3x3
-- |  |a -> b| = |b| x |b| x .. x |b| = |b|^|a|

emptyBoard :: TicTacToe (Maybe Bool)
emptyBoard = TicTacToe
  Nothing Nothing Nothing
  Nothing Nothing Nothing
  Nothing Nothing Nothing

data Three = One | Two | Three
  deriving (Eq, Ord, Enum, Bounded)

data TicTacToe2 a = TicTacToe2
  { board :: Three -> Three -> a
  }

emptyBoard2 :: TicTacToe2 (Maybe Bool)
emptyBoard2 =
  TicTacToe2 $ const $ const Nothing
  
  
