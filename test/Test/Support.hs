--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
module Test.Support
  ( withTestDatabase
  ) where
--------------------------------------------------------------------------------
import Test.Prelude
--------------------------------------------------------------------------------
import Xndr
--------------------------------------------------------------------------------
import Control.Monad.Except
--------------------------------------------------------------------------------
import qualified Database.HDBC as SQL
--------------------------------------------------------------------------------
import Database.HDBC.Sqlite3 (connectSqlite3)
import Hedgehog (Gen, Range)
--------------------------------------------------------------------------------
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
--------------------------------------------------------------------------------
import System.Directory (removeFile)
import System.IO.Unsafe (unsafePerformIO)

testState :: MVar Int
{-# NOINLINE testState #-}
testState = unsafePerformIO $ newMVar 0

newTestState :: MonadIO m => m Int
newTestState = liftIO $ modifyMVar testState (\x -> pure (succ x, x))

mkTestDatabase :: Int -> String
mkTestDatabase state = "testDatabase" <> show state <> ".db"

initDatabase :: FilePath -> IO ()
initDatabase dbPath = do
  conn <- connectSqlite3 dbPath
  void . SQL.withTransaction conn $ \c ->
    SQL.run c ("CREATE TABLE queues (id varchar(36) PRIMARY KEY, " <>
    "name text UNIQUE, desc text)") []

withTestDatabase
  :: XndrM a -> IO a
withTestDatabase xndrM = do
  _dbFile <- mkTestDatabase <$> newTestState
  let
    _dbDir = "/tmp/"
    dbPath = _dbDir <> _dbFile

  initDatabase dbPath

  -- Execute and extract the value
  retVal <- runReaderT xndrM Env{..}

  liftIO $ removeFile dbPath
  pure retVal
