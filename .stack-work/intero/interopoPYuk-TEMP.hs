{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications  #-}

module WorkingWithTypes where


-- | -XScopedTypeVariables
working :: forall a b. (a -> b) -> a -> b
-- ^ forall a b. quantifier introduces a type scope and exposes
-- the type variable a and b to remainder of the function definition.:set -XTypeApplications
working f a = apply
  where
    apply :: b
    apply = f a 
