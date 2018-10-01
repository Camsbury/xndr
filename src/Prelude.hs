--------------------------------------------------------------------------------
module Prelude
  ( module ClassyPrelude
  , throwMaybe
  , throwEither
  ) where
--------------------------------------------------------------------------------
import ClassyPrelude
import Control.Monad.Except
--------------------------------------------------------------------------------


-- | throws error if Nothing
throwMaybe :: MonadError e m => e -> Maybe a -> m a
throwMaybe e = maybe (throwError e) pure

-- | throws error if Nothing
throwEither :: MonadError e m => Either e a -> m a
throwEither = either throwError pure
