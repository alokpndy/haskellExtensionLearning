{-# LANGUAGE GADTs                #-}

module ConstraintsAndGADTs where


five :: Int
five = 5
-- Type equalities
five_ :: (a ~ Int) => a
-- ^ Identical as five  
five_ = 5

{-- Type equalities form an equivalence relation, meaning
 -- reflexivity—a type is always equal to itself: a ∼ a
 -- symmetry—a ∼bholdsifandonlyifb ∼a
 -- transitivity—if we know both a ∼ b and b ∼ c,
    we (and GHC) can infer that a ∼ c.
--}


-- Generalised Algebraic Data Type
data Exp a where
    I :: Int -> Exp Int
    B :: Bool -> Exp Bool
    Add :: Exp Int -> Exp Int -> Exp Int
    Mul :: Exp Int -> Exp Int -> Exp Int
    Equ :: Exp Int -> Exp Int -> Exp Bool

eval :: Exp a -> a
eval (I n)       = n
eval (B b)       = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Equ e1 e2) = eval e1 ==  eval e2
