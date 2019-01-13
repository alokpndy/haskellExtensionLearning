module Algebra where


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


