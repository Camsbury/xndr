--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
module Test.Support
  ( newTestState
  , mkTestFileName
  ) where
--------------------------------------------------------------------------------
import Test.Prelude
--------------------------------------------------------------------------------
import System.IO.Unsafe (unsafePerformIO)

testState :: MVar Int
{-# NOINLINE testState #-}
testState = unsafePerformIO $ newMVar 0

newTestState :: MonadIO m => m Int
newTestState = liftIO $ modifyMVar testState (\x -> pure (succ x, x))

mkTestFileName :: Int -> String
mkTestFileName state = "testQueue" <> "." <> show state
