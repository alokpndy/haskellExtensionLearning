{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module WorkingWithTypes where

-- # Imports
import Data.Typeable


-- | -XScopedTypeVariables
-- notWorking :: (a -> b) -> a -> b
working :: forall a b. (a -> b) -> a -> b
-- ^ forall a b. quantifier introduces a type scope and exposes
-- the type variable a and b to remainder of the function definition.
-- :set -XTypeApplications
working f a = apply
  where
    apply :: b
    apply = f a 


-- | TypeAppplication
-- allow to directly apply typees to expressions by fixing @
--  :t fmap @Maybe
-- fmap @Maybe :: (a -> b) -> Maybe a -> Maybe b

data Foo f = Foo { foo :: f String, bar :: f Int }





{--
There are two rules to keep in mind when thining about type applications.
1. types are applied in the same order they appear in a type signature
   including its context and forall quantifiers.
This means that applying a type Int to
        a -> b -> a results in Int -> b -> Int.
But type applying itto
        forall b a. a -> b -> a is infact a -> Int -> a.

2. The second rule of type applications is that you can avoid applying a type
    with an underscore: @_
:t fmap @_ @Int @Bool
--fmap @_ @Int @Bool :: Functor w => (Int -> Bool) -> w Int -> w Bool
--}


-- # Ambigous type and Non Injectivity
{--
  -XAllowAmbiguousTypes allows us to define ambiguously typed functions, and
  -XTypeApplications enables us to call them.
--}

-- as a do not occur on right of => , a is ambigous
typeName :: forall a. Typeable a => String
typeName = show . typeRep $ Proxy @a
--                  ^  Again, the Proxy’s only purpose is to let typeRep
                    -- know which type representation we’re looking for.
                    -- As such, typeRep has to be called as
                    -- typeRep (Proxy :: Proxy Bool)
-- Proxy@   shorthand for Proxy :: Proxy a
--  we have type applied   Proxy t    to Proxy a 
-- data Proxy t = Proxy    t is phantom type


-- | type family AlwaysUnit a where
--      AlwaysUnit a = ()

-- since   Show a => AlwaysUnit a -> String  is non injective
-- i.e there is no inverse we cannot find what a is from AlwaysUnit a


sizeOf  :: forall a. Show a =>  Int
sizeOf  =  3

