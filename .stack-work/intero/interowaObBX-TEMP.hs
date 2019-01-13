{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications  #-}

{-# LANGUAGE RankNTypes #-}

module RankN where

-- | Endomorphism - The functions which takes and return the same type
-- eg not :: Bool -> Bool, show @String :: String -> String

id_ :: forall a. a -> a
id_ a = a 

applyToFive :: (forall a. a -> a) -> Int
-- ^ Using RankNTypes we seperate forall inside bracket
applyToFive f = f 5
-- applytoFive = id_

