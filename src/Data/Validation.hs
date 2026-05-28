{- | Applicative-only validation that accumulates errors via the 'Semigroup'
on @e@. No 'Monad' instance: the @ap = (\<*\>)@ law would force '<*>' to
short-circuit, destroying accumulation.

@
    validateAll
        :: Maybe Text -> Maybe Text -> Validation (NonEmpty Text) (Foo, Bar)
    validateAll a b = (,) \<$\> validateA a \<*\> validateB b
@

When both arms fail, the resulting 'Failure' carries both messages.
-}
module Data.Validation (
    Validation (..),
    toEither,
    fromEither,
    failure,
    success,
) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

-- | @Failure e@ accumulates via @e@'s 'Semigroup'; @Success a@ propagates.
data Validation e a
    = Failure !e
    | Success !a
    deriving (Eq, Show)

instance Functor (Validation e) where
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success (f a)

{- | The 'Applicative' instance is the whole point: when both arguments
are 'Failure', their error payloads are combined via @(\<\>)@. This is the
defining behaviour that distinguishes 'Validation' from 'Either'.
-}
instance Semigroup e => Applicative (Validation e) where
    pure = Success
    Failure e1 <*> Failure e2 = Failure (e1 <> e2)
    Failure e <*> Success _ = Failure e
    Success _ <*> Failure e = Failure e
    Success f <*> Success a = Success (f a)

-- | Convert to 'Either' once accumulation is done (e.g. at a Servant boundary).
toEither :: Validation e a -> Either e a
toEither (Failure e) = Left e
toEither (Success a) = Right a

-- | Inject an 'Either' into 'Validation'. Use 'fromEither' to lift a
-- short-circuiting parser into an accumulating context.
fromEither :: Either e a -> Validation e a
fromEither (Left e) = Failure e
fromEither (Right a) = Success a

-- | Build a singleton-error failure. Convenient for 'NonEmpty'-keyed
-- validators where each leaf check produces exactly one error message.
failure :: e -> Validation (NonEmpty e) a
failure = Failure . NE.singleton

-- | Inject a value into 'Success'. Symmetric to 'failure'; useful in
-- chains where readability is helped by an explicit constructor.
success :: a -> Validation e a
success = Success
