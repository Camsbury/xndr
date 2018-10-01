module Test.Prelude
  ( module Prelude
  , module Test.Tasty
  , module Test.Tasty.Hedgehog
  , module Hedgehog
  , unsafeEither
  ) where

import Prelude hiding (assert)
import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog

-- | Document me!
unsafeEither :: Show e => Either e a -> a
unsafeEither = either (error . show) id
