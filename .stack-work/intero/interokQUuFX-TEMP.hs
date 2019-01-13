module Variance where

-- | Covariant
{-- Any function a -> b can be lifted into a function T a -> T b.--}

-- | Contravariant
{-- Any function a -> b can be lifted into a function T b -> T a.--}

-- | Invariant
{--In general, no function a -> b can be lifted into a function over T a.--}

-- | A type T is a functor if and only if it is covariant.

{-- A type that is covariant in two arguments (like Either and (,))
    is called a bifunctor.
    A type that is con- travariant in its first argument,
    but covariant in its second (like (->)) is known as a profunctor.
--}
