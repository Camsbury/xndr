{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module: Xndr
-- Description: The main loop for xndr.
-- Maintainers: Cameron Kingsbury <cameron@urbint.com>
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
--------------------------------------------------------------------------------
import Data.Store (Store, encode, decode)
import System.Environment (lookupEnv)
import System.Directory (doesFileExist, createDirectoryIfMissing)
--------------------------------------------------------------------------------
import Control.Lens
  ( ix
  , makeFieldsNoPrefix
  , view
  , _Just
  )
--------------------------------------------------------------------------------
-- Types
-- TODO: clean these up to be derived from 'CmdTag'

-- | Beginning representation of the Xndr priority queue
newtype XndrQueue
  = XndrQueue
  { _innerQueue :: Vector Text
  }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid, Store)

makeFieldsNoPrefix ''XndrQueue

-- | May turn into a newtype soon, but alias first for simplicity
type Topic = Text

-- | Sum type of the commands available for xndr
data XndrCmd
  = Top
  | Pop
  | Insert Topic
  | Info Topic
  | Describe Topic Text
  deriving stock (Eq, Show)

-- | Sum type of command names
data CmdTag
  = TopTag
  | PopTag
  | InsertTag
  | InfoTag
  | DescribeTag
  deriving stock (Eq, Show, Ord, Bounded)

-- | Sum type of the reducer's actions
data XndrAction
  = DoSwap Int Int
  | DoAppend Topic
  | DoPopTail
  | DoDescribe Topic Text
  deriving stock (Eq, Show)

newtype ReducerError
  = IndexOutOfRange Int
  deriving stock (Eq, Show)

data Env
  = Env
  { _queueFileName :: String
  , _queueDir      :: FilePath
  , _queueRef      :: IORef XndrQueue
  , _compTable     :: Maybe (HashMap Topic (HashMap Topic Bool))
  }
makeFieldsNoPrefix ''Env

queuePath
  :: ( HasQueueFileName env String
    , HasQueueDir env FilePath
    , MonadReader env m
    )
  => m FilePath
queuePath = (<>) <$> view queueDir <*> view queueFileName


type XndrM = ReaderT Env IO


--------------------------------------------------------------------------------
-- Main function

-- | Takes commmand line arguments, and performs the corresponding xndr action.
xndr :: [Text] -> IO ()
xndr rawCmd = do
  homeDir <- fromMaybe "/tmp" <$> lookupEnv "HOME"
  _queueRef <- newIORef mempty
  let
    _queueDir = homeDir <> "/.xndr/"
    _queueFileName = "default"
    _compTable = mempty
  (`runReaderT` Env{..}) $ do
    readQueueFile
    maybe handleNothing handleCmd . parseCmd $ rawCmd
    where
      handleNothing
        = print . unwords
        $ "Invalid argument. Valid arguments include:" : cmdList


--------------------------------------------------------------------------------
-- File Management

-- | Get the queue from the environment
readQueueFile :: XndrM ()
readQueueFile = do
  path <- queuePath
  qRef <- view queueRef
  exists <- liftIO $ doesFileExist path
  queue <- bool (pure mempty) (readQueue path) exists
  writeIORef qRef queue
  where
    readQueue :: FilePath -> XndrM XndrQueue
    readQueue path
      =   liftIO
      $   either (const mempty) id . decode
      <$> readFile path

-- | Write the current queue from memory
writeQueueFile :: XndrM ()
writeQueueFile = do
  path <- queuePath
  exists <- liftIO $ doesFileExist path
  dir <- view queueDir
  bool (liftIO $ createDirectoryIfMissing True dir) (pure ()) exists
  queue <- getQueue
  writeFile path $ encode queue


--------------------------------------------------------------------------------
-- Queue Manipulation

-- | Get the current queue in memory
getQueue :: XndrM XndrQueue
getQueue = readIORef =<< view queueRef

-- | Write to the queue in memory
writeQueue :: XndrQueue -> XndrM ()
writeQueue queue = (`writeIORef` queue) =<< view queueRef

-- | Get the current queue in memory
modifyQueue :: (XndrQueue -> XndrQueue) -> XndrM ()
modifyQueue f = (`modifyIORef` f) =<< view queueRef

-- | Get the current queue in memory
emptyQueue :: XndrM ()
emptyQueue = writeQueue mempty


--------------------------------------------------------------------------------
-- Command Execution

-- | Parses a textual command into a 'XndrCmd'
parseCmd :: [Text] -> Maybe XndrCmd
parseCmd
  = \case
      ["top"]
        -> Just Top

      ["pop"]
        -> Just Pop

      ["insert", topic]
        -> Just $ Insert topic

      ["info", topic]
        -> Just $ Info topic

      ["describe", topic, description]
        -> Just $ Describe topic description

      _
        -> Nothing


-- | Possible commands for xndr
cmdList :: [Text]
cmdList =
  [ "top"
  , "pop"
  , "insert"
  , "info"
  , "describe"
  ]


-- | Handles a successfully parsed Command
handleCmd :: XndrCmd -> XndrM ()
handleCmd Top =
  liftIO . putStrLn =<< (maybe noTop topText . queryTop <$> getQueue)
  where
    topText x = "\"" <> x <> "\" is the most important topic in the queue."
    noTop     = "Nothing in the queue!"

handleCmd (Insert topic)
  =   either putError handleSuccess
  =<< runExceptT (mutationInsert topic)
  where
    handleSuccess :: XndrQueue -> XndrM ()
    handleSuccess queue = do
      writeQueue queue
      writeQueueFile
      putStrLn $ "\"" <> topic <> "\" inserted successfully."

    putError :: ReducerError -> XndrM ()
    putError err
      = putStrLn
      $ "Error encountered while inserting: " <> tshow err

handleCmd _ = liftIO $ putStrLn "This action isn't handled yet!"


-- -- | The response function for each command
-- xndrResponse :: XndrQueue -> XndrCmd -> Text
-- xndrResponse queue
--   = \case
--       Pop
--         -> "Popped \"topic1\" from the queue."

--       Insert topic
--         -> "\"" <> topic <> "\" was inserted into the queue."

--       Info topic
--         -> "\"" <> topic <> "\": descriptions of the topic go here!"

--       Describe topic description
--         -> "\"" <> topic <> "\" description added"


--------------------------------------------------------------------------------
-- Queries

queryTop :: XndrQueue -> Maybe Topic
queryTop queue = queue ^? innerQueue . ix 0


--------------------------------------------------------------------------------
-- Mutations

mutationInsert :: Topic -> ExceptT ReducerError XndrM XndrQueue
mutationInsert topic = do
  queue' <- lift getQueue
  queue <- throwEither $ reduceXndr (DoAppend topic) queue'
  bubbleUp queue $ lastIndex queue
  where
    lastIndex q = length (q ^. innerQueue) - 1
    bubbleUp :: XndrQueue -> Int -> ExceptT ReducerError XndrM XndrQueue
    bubbleUp q n
      | n < 0 = error "Improper parent calculation in the XndrQueue"
      | n == 0 = pure q
      | otherwise = do
          cTable <- lift $ view compTable
          let
            parent = getParent n
            -- FIXME: These vals will error if everything is horribly wrong
            -- should property test
            nVal      = q ^?! innerQueue . ix n
            parentVal = q ^?! innerQueue . ix parent
            maybeComp = cTable ^? _Just . ix nVal . ix parentVal
          comp <- maybe (getComp nVal parentVal) pure maybeComp
          bool (pure q) (continueBubble n parent q) comp

    continueBubble
      :: Int
      -> Int
      -> XndrQueue
      -> ExceptT ReducerError XndrM XndrQueue
    continueBubble n parent q
      = (`bubbleUp` parent) =<< throwEither (reduceXndr (DoSwap n parent) q)

    getComp
      :: Topic
      -> Topic
      -> ExceptT ReducerError XndrM Bool
    getComp child parent = do
      liftIO
        . putStrLn
        $ "Is \""
        <> child
        <> "\" more important than \""
        <> parent
        <> "\"? (y/n)"
      line <- getLine
      case line of
        "y" -> pure True
        "n" -> pure False
        _     -> getComp child parent


--------------------------------------------------------------------------------
-- Reducer

-- | Reduce a 'XndrAction' into the 'XndrQueue'
reduceXndr :: XndrAction -> XndrQueue -> Either ReducerError XndrQueue
reduceXndr action queue
  = case action of
      DoPopTail
        -> pure $ queue & innerQueue %~ (fromMaybe mempty . initMay)

      DoAppend topic
        -> pure $ queue & innerQueue %~ (`snoc` topic)

      DoSwap x y -> do
        let
          mXVal = queue ^? innerQueue . ix x
          mYVal = queue ^? innerQueue . ix y
        xVal <- throwMaybe (IndexOutOfRange x) mXVal
        yVal <- throwMaybe (IndexOutOfRange x) mYVal
        pure $ queue &~ do
          innerQueue . ix x .= yVal
          innerQueue . ix y .= xVal

      -- FIXME: currentlly a noop
      DoDescribe _topic _desc
        -> pure queue


--------------------------------------------------------------------------------
-- Utility

getParent :: Int -> Int
getParent n
  | n > 0     = div (n - 1) 2
  | otherwise = 0
