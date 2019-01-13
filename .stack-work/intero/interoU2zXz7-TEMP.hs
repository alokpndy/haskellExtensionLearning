module GADTsy where

{-# LANGUAGE GADTs #-}
-- | Toy instructional example of GADTs: simple dynamic types.
--
-- Before tackling this, skim the GHC docs section on GADTs:
--
-- <http://www.haskell.org/ghc/docs/latest/html/users_guide/data-type-extensions.html#gadt>
--
-- As you read this example keep in mind this quote from the
-- docs: "The key point about GADTs is that /pattern matching 
-- causes type refinement/."  That will indeed turn out to be
-- the key point, repeatedly.

import Control.Applicative
----------------------------------------------------------------
----------------------------------------------------------------
--
-- Type representations
--
-- | This GADT gives us values that stand in for certain types.
-- (Obvious limitation; the set of types is fixed at compilation time.)
--
-- If @x :: TypeRep a@, then @x@ is a singleton value that stands in
-- for type @a@.  This works because of the GADT "pattern matching
-- refines types" rule; for example, if a pattern successfully matches
-- against the 'Integer' constructor for this type, then it must be the
-- case that @a = Integer@.  So even if your function accepts @TypeRep a@,
-- in the body of that particular pattern match you get @a = Integer@
-- instead of just @a@.
data TypeRep a where 
Integer :: TypeRep Integer
Char    :: TypeRep Char
Maybe   :: TypeRep a -> TypeRep (Maybe a)
List    :: TypeRep a -> TypeRep [a]
Pair    :: TypeRep a -> TypeRep b -> TypeRep (a, b)
-- | Typeclass for types that have a 'TypeRep'.
class Representable a where
typeRep :: TypeRep a
instance Representable Integer where typeRep = Integer
instance Representable Char where typeRep = Char
instance Representable a => Representable (Maybe a) where 
    typeRep = Maybe typeRep
instance Representable a => Representable [a] where 
    typeRep = List typeRep
instance (Representable a, Representable b) => Representable (a, b) where 
    typeRep = Pair typeRep typeRep
----------------------------------------------------------------
----------------------------------------------------------------
--
-- Equality proofs
--
-- | This second GADT represents type equality proofs.  If we have
-- @Refl :: Equal a b@, then actually it must be the case that @a = b@.
-- This is guaranteed by the type of the 'Refl' constructor.
data Equal a b where
Refl :: Equal a a
-- | Structural induction over types and type constructors.  You can
-- read the type as a proposition: if @a@ and @b@ are equal, then @f
-- a@ and @f b@ are also so.
induction :: Equal a b -> Equal (f a) (f b)
induction Refl = Refl
induction2 :: Equal a b -> Equal c d -> Equal (f a c) (f b d)
induction2 Refl Refl = Refl
-- | If we have an @Equal a b@, we can actually turn @a@ into @b@.
--
-- The trick to this is the heart of GADTs: successful matches cause
-- types to be refined.  Since 'Refl' can only be constructed if @a = b@,
-- when we successfully pattern match on 'Refl' the body of the function
-- executes in a type environment where @a = b@.
cast :: Equal a b -> a -> b
cast Refl a = a
-- | Bringing 'TypeRep' and 'Equal' together: match two 'TypeRep's, and if
-- they represent the same type return a proof that the types are 'Equal'.
--
-- Note that you can only successfully return 'Just Refl' in cases where
-- the two 'TypeRep's are the same.  So this code is ill-typed:
--
-- > matchTypes Integer Char = Just Refl
--
-- Matching on 'Integer' and 'Char' refines @a := Integer@ and 
-- @b := Char@, but @Refl :: Equal Integer Char@ is ill-typed.
matchTypes :: TypeRep a -> TypeRep b -> Maybe (Equal a b)
matchTypes Integer Integer = Just Refl
matchTypes Char Char = Just Refl
matchTypes (List a) (List b) = induction <$> matchTypes a b
matchTypes (Maybe a) (Maybe b) = induction <$> matchTypes a b
matchTypes (Pair a b) (Pair c d) = 
    induction2 <$> matchTypes a c <*> matchTypes b d
matchTypes _ _ = Nothing
----------------------------------------------------------------
----------------------------------------------------------------
--
-- Dynamic data
--
-- | A 'Dynamic' is a just a pair that contains a 'TypeRep' and a
-- value of the type that the 'TypeRep' stands for.
--
-- This illustrates yet another power of GADTs: the type variable
-- @a@ that appears in the 'Dyn' constructor does not appear in
-- the 'Dynamic' type.  You're getting existential quantification.
data Dynamic where
Dyn :: TypeRep a -> a -> Dynamic
-- | Inject a value of a 'Representable' type into 'Dynamic'.
toDynamic :: Representable a => a -> Dynamic
toDynamic = Dyn typeRep
-- | Cast a 'Dynamic' into a 'Representable' type.
fromDynamic :: Representable a => Dynamic -> Maybe a
fromDynamic (Dyn fromType value) =
flip cast value <$> matchTypes fromType typeRep