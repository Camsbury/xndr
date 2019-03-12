{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module: Xndr
-- Description: The API for Xndr's queue.
-- Maintainers: Cameron Kingsbury <camsbury7@gmail.com>
-- Maturity: Draft
--
--
-- = Usage Example
-- @xndr ["top"]@ will print the top priority item in the store.
--
--------------------------------------------------------------------------------
module Xndr
  ( module Xndr
  ) where
--------------------------------------------------------------------------------
import Prelude
--------------------------------------------------------------------------------
import Control.Lens.Operators
import Control.Monad.Except
import Control.Monad.State
--------------------------------------------------------------------------------
import qualified Database.HDBC as SQL
import qualified Data.UUID     as UUID
import qualified Data.UUID.V4  as UUID
--------------------------------------------------------------------------------
import Database.HDBC (SqlValue(..), SqlError(..))
import Database.HDBC.Sqlite3 (connectSqlite3)
import Data.Store (Store, encode, decode)
import Data.Vector (elemIndex)
import System.Environment (lookupEnv)
import System.Directory
  ( doesFileExist
  , createDirectoryIfMissing
  , listDirectory
  )
--------------------------------------------------------------------------------
import Control.Lens
  ( ix
  , makeFieldsNoPrefix
  , view
  , folded
  , to
  )
--------------------------------------------------------------------------------
-- Types

-- | May turn into a newtype soon, but alias first for simplicity
type QueueName = Text
type Description = Text

-- | Sum type of the commands available for xndr
data XndrCmd
  = QueueList
  | QueueCreate QueueName (Maybe Description)
  | QueueDelete QueueName
  | QueueInfo QueueName
  | QueueDesc QueueName Description
  deriving stock (Eq, Show)

-- | Sum type of the commands available for xndr
data XndrResponse
  = QueueListResponse [QueueName]
  | QueueCreateSuccess QueueName
  | QueueCreateExists  QueueName
  | QueueDeleteSuccess QueueName
  | QueueInfoSuccess   QueueName Description
  | QueueInfoEmpty     QueueName
  | QueueInfoMissing   QueueName
  | QueueDescSuccess   QueueName
  | QueueDescMissing   QueueName
  deriving stock (Eq, Show)

-- | Sum type of the reducer's actions
data XndrAction
  = DoQueueCreate QueueName (Maybe Description)
  | DoQueueDelete QueueName
  | DoQueueDesc   QueueName Description
  deriving stock (Eq, Show)

-- | The Env that pure functions pull from
data Env
  = Env
  { _dbDir  :: FilePath
  , _dbFile :: FilePath
  }
makeFieldsNoPrefix ''Env

-- | Gets the path for the DB
dbPath
  :: ( HasDbDir  env FilePath
    , HasDbFile env FilePath
    , MonadReader env m
    ) => m FilePath
dbPath = mappend <$> view dbDir <*> view dbFile

-- | App Monad for Xndr
type XndrM = ReaderT Env IO


--------------------------------------------------------------------------------
-- Main function

-- | Performs the commands given in 'XndrCmd'!
xndr :: XndrCmd -> IO ()
xndr xndrCmd = SQL.handleSqlError $ do
  homeDir <- fromMaybe "/tmp" <$> lookupEnv "HOME"
  let
    _dbDir  = homeDir <> "/.xndr/"
    _dbFile = "default.db"

  displayResponse =<< runReaderT (handleCmd xndrCmd) Env{..}
    where
      displayResponse :: XndrResponse -> IO ()
      displayResponse (QueueListResponse [])
        = putStrLn "There are no queues available"
      displayResponse (QueueListResponse queues) = do
        putStrLn "Available Queues:\n"
        traverse_ putStrLn queues
      displayResponse (QueueCreateSuccess queueName)
        = putStrLn $ "Queue \"" <> queueName <> "\" created."
      displayResponse (QueueCreateExists queueName) =
        putStrLn $ "Queue \"" <> queueName <> "\" already exists."
      displayResponse (QueueDeleteSuccess queueName) =
        putStrLn $ "Queue \"" <> queueName <> "\" deleted (if it existed)."
      displayResponse (QueueInfoSuccess queueName description) =
        putStrLn $ "Queue \"" <> queueName <> "\":\n" <> description
      displayResponse (QueueInfoEmpty queueName) =
        putStrLn $ "Queue \"" <> queueName <> "\" has no description."
      displayResponse (QueueInfoMissing queueName) =
        putStrLn $ "Queue \"" <> queueName <> "\" doesn't exist."
      displayResponse (QueueDescSuccess queueName) =
        putStrLn $ "Queue \"" <> queueName <> "\" described successfully."
      displayResponse (QueueDescMissing queueName) =
        putStrLn $ "Queue \"" <> queueName <> "\" doesn't exist."

--------------------------------------------------------------------------------
-- Command Execution

-- | Dispatches a `XndrCmd`
handleCmd :: XndrCmd -> XndrM XndrResponse
handleCmd QueueList = do
  conn <- lift . connectSqlite3 =<< dbPath
  queues <- lift . SQL.withTransaction conn $ \c ->
    SQL.quickQuery c "SELECT name FROM queues" []
  pure . QueueListResponse $ foldr convertName [] queues
  where
    convertName :: [SqlValue] -> [QueueName] -> [QueueName]
    convertName [name] tail = pack (SQL.fromSql name) : tail
    convertName _      tail = tail
handleCmd (QueueCreate queueName mDesc) =
  handleMutation (DoQueueCreate queueName mDesc)
handleCmd (QueueDelete queueName) =
  handleMutation (DoQueueDelete queueName)
handleCmd (QueueInfo queueName) = do
  conn <- lift . connectSqlite3 =<< dbPath
  description <- lift . SQL.withTransaction conn $ \c ->
    SQL.quickQuery c "SELECT desc FROM queues WHERE name = ? LIMIT 1"
      [SQL.toSql $ unpack queueName]
  pure $ extract description
  where
    extract :: [[SqlValue]] -> XndrResponse
    extract [[SqlNull]] = QueueInfoEmpty queueName
    extract [[desc]]    = QueueInfoSuccess queueName . pack $ SQL.fromSql desc
    extract _           = QueueInfoMissing queueName
handleCmd (QueueDesc queueName desc) =
  handleMutation (DoQueueDesc queueName desc)


-- | Dispatches a `XndrAction`
handleMutation
  :: XndrAction
  -> XndrM XndrResponse
handleMutation (DoQueueCreate queueName mDesc) = do
  conn <- lift . connectSqlite3 =<< dbPath
  queueID <- lift UUID.nextRandom
  lift . SQL.withTransaction conn $ \c ->
    (`SQL.catchSql` catchDuplicateQueue) $ do
      SQL.run c "INSERT INTO queues (id, name, desc) VALUES (?, ?, ?)"
        [ SQL.toSql $ UUID.toString queueID
        , SQL.toSql $ unpack queueName
        , maybe SqlNull SQL.toSql mDesc
        ]
      pure $ QueueCreateSuccess queueName
  where
    catchDuplicateQueue (SqlError _ 19 _) = pure $ QueueCreateExists queueName
    catchDuplicateQueue err = SQL.throwSqlError err
handleMutation (DoQueueDelete queueName) = do
  conn <- lift . connectSqlite3 =<< dbPath
  lift . SQL.withTransaction conn $ \c ->
      SQL.run c "DELETE FROM queues WHERE name = ?"
        [SQL.toSql $ unpack queueName]
  pure $ QueueDeleteSuccess queueName
handleMutation (DoQueueDesc queueName desc) = do
  conn <- lift . connectSqlite3 =<< dbPath
  queue <- lift . SQL.withTransaction conn $ \c -> do
      SQL.run c "UPDATE queues SET desc = ? WHERE name = ?"
        -- (SQL.toSql . unpack <$> [desc, queueName])
        [SQL.toSql $ unpack desc, SQL.toSql $ unpack queueName]
      SQL.quickQuery c "SELECT COUNT(*) FROM queues WHERE name = ?"
        [SQL.toSql $ unpack queueName]
  pure $ ensure queue
  where
    ensure :: [[SqlValue]] -> XndrResponse
    ensure [[count]]
      | (SQL.fromSql count :: Int) == 1 = QueueDescSuccess queueName
      | otherwise                     = QueueDescMissing queueName
