module FourDashOneWorkingWIthTyoes where



---
class Test a where
  toInt :: a ->  Int 
  -- toInt2 :: Int will not work as   -- (1) 

-- But toInt should be constant and same for all a
-- we dont need to write instance for all data like below
instance Test Bool where
  toInt x = 4

  -- | (1) 
 {- toInt2  :: forall a. Test a => Int.
The problem is type inference. If I write wrong somewhere,
the inference algorithm only knows that I expect an Int.
It has no idea what type I want to substitute for a.
Because of this, the definition gets rejected by the compiler
unless you have the {-# LANGUAGE AllowAmbiguousTypes #-}
-}

-- Remedy 2
data Proxy a = Proxy

class Text2 a where
  better :: Proxy a -> Int 

-- | WIth TypeApplications we can write toInt @Int 




