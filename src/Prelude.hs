{-# OPTIONS_GHC -fno-warn-deprecations #-}
--------------------------------------------------------------------------------
module Prelude
  ( module ClassyPrelude
  -- | Basic
  , allUnique
  -- | Logging
  , inspect
  , inspectM
  -- | MonadError
  , throwMaybe
  , throwEither
  -- | Generators
  , genUniqueList
  ) where
--------------------------------------------------------------------------------
import ClassyPrelude
import Control.Monad.Except
--------------------------------------------------------------------------------
import Hedgehog (Gen, Range)
--------------------------------------------------------------------------------
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
--------------------------------------------------------------------------------
-- Basic

-- | Checks that all values are unique
allUnique :: (Eq a) => [a] -> Bool
allUnique [] = True
allUnique (x:xs)
  = x `notElem` xs && allUnique xs


--------------------------------------------------------------------------------
-- Logging

-- | Prints a value annotated with the passed label and returns the input value.
inspect :: (Show a) => String -> a -> a
inspect label x = trace (label ++ ": " ++ show x) x
{-# WARNING inspect "'inspect' remains in code" #-}

-- | Prints a value annotated with the passed label and returns a monadic unit
inspectM :: (Show a, Monad m) => String -> a -> m ()
inspectM label x = traceM (label ++ ": " ++ show x)
{-# WARNING inspectM "'inspectM' remains in code" #-}


--------------------------------------------------------------------------------
-- MonadError

-- | throws error if Nothing
throwMaybe :: MonadError e m => e -> Maybe a -> m a
throwMaybe e = maybe (throwError e) pure

-- | throws error if Nothing
throwEither :: MonadError e m => Either e a -> m a
throwEither = either throwError pure


 --------------------------------------------------------------------------------
 -- Generators

-- | Generates a unique list
genUniqueList :: Eq a => Range Int -> Gen a -> Gen [a]
genUniqueList range gen = do
  Gen.filter allUnique $ Gen.list range gen
