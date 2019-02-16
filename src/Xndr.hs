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
import Data.Store (Store, encode, decode)
import Data.Vector (elemIndex)
import System.Environment (lookupEnv)
import System.Directory (doesFileExist, createDirectoryIfMissing)
--------------------------------------------------------------------------------
import Control.Lens
  ( ix
  , makeFieldsNoPrefix
  , view
  , folded
  )
--------------------------------------------------------------------------------
-- Types

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
  | List
  | Delete Topic
  | Insert Topic
  | Info Topic
  | Describe Topic Text
  deriving stock (Eq, Show)

-- | Sum type of the reducer's actions
data XndrAction
  = DoSwap Int Int
  | DoAppend Topic
  | DoPopTail
  | DoDescribe Topic Text
  deriving stock (Eq, Show)

-- | Sum type of the reducer's errors
data ReducerError
  = IndexOutOfRange Int
  | TopicNotInQueue Topic
  deriving stock (Eq, Show)

-- | The Env that pure functions pull from
data Env
  = Env
  { _queueFileName :: String
  , _queueDir      :: FilePath
  , _queueRef      :: IORef XndrQueue
  , _compFn        :: Maybe (Topic -> Topic -> Bool)
  }
makeFieldsNoPrefix ''Env

-- | The file path for the XndrQueue
queuePath
  :: ( HasQueueFileName env String
    , HasQueueDir env FilePath
    , MonadReader env m
    )
  => m FilePath
queuePath = (<>) <$> view queueDir <*> view queueFileName

-- | App Monad for Xndr
type XndrM = ReaderT Env IO


--------------------------------------------------------------------------------
-- Main function

-- | Performs the commands given in XndrCmd!
xndr :: XndrCmd -> IO ()
xndr cmd = do
  -- Create the Environmment
  homeDir <- fromMaybe "/tmp" <$> lookupEnv "HOME"
  _queueRef <- newIORef mempty
  let
    _queueDir = homeDir <> "/.xndr/"
    _queueFileName = "default"
    _compFn = Nothing

  -- Run in the Environment
  (`runReaderT` Env{..}) $ do
    -- Read the `XndrQueue` from the File System
    readQueueFile
    -- Handle the `XndrCmd`
    handleCmd cmd


--------------------------------------------------------------------------------
-- Queue File Management

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

-- | Dispatches a `XndrCmd`
handleCmd :: XndrCmd -> XndrM ()
handleCmd Top
  = maybe emptyQueueMsg printTop . queryTop =<< getQueue
  where
    printTop :: Text -> XndrM ()
    printTop x
      = putStrLn
      $ "\"" <> x <> "\" is the highest priority topic in the queue."

handleCmd List
  = traverse_ putStrLn . createDisplayTree . queryList =<< getQueue

handleCmd Pop = do
  queue <- getQueue
  maybe emptyQueueMsg handlePop $ queryTop queue
  where
    handlePop :: Topic -> XndrM ()
    handlePop topic
      = handleMutation "pop" topic mutationDelete

handleCmd (Insert topic)
  = handleMutation "insert" topic mutationInsert

handleCmd (Delete topic)
  = handleMutation "delete" topic mutationDelete

handleCmd _ = liftIO $ putStrLn "This action isn't handled yet!"

-- | Generic mutation handler for a topic given an action
handleMutation
  :: Text
  -> Topic
  -> (Topic -> ExceptT ReducerError XndrM XndrQueue)
  -> XndrM ()
handleMutation action topic mutation
  = either printError onCmdSuccess
  =<< runExceptT (mutation topic)
  where
    onCmdSuccess :: XndrQueue -> XndrM ()
    onCmdSuccess queue = do
      writeQueue queue
      writeQueueFile
      putStrLn $ "\"" <> topic <> "\" " <> action <> " successful."

    printError :: ReducerError -> XndrM ()
    printError err
      = putStrLn
      $ "Error encountered while attempting to " <> action <> ": " <> tshow err


--------------------------------------------------------------------------------
-- Queries

queryTop :: XndrQueue -> Maybe Topic
queryTop queue = queue ^? innerQueue . ix 0

queryList :: XndrQueue -> [Topic]
queryList queue = queue ^.. innerQueue . folded


--------------------------------------------------------------------------------
-- Mutations

mutationDelete :: Topic -> ExceptT ReducerError XndrM XndrQueue
mutationDelete topic = do
  queue <- lift getQueue
  let
    lastIdx = length (queue ^. innerQueue) - 1
    mIndex = elemIndex topic $ queue ^. innerQueue
  idx <- throwMaybe (TopicNotInQueue topic) mIndex
  bubbleDown idx
    <=< throwEither
    $   reduceXndr DoPopTail
    <=< reduceXndr (DoSwap idx lastIdx)
    $   queue
  where
    bubbleDown :: Int -> XndrQueue -> ExceptT ReducerError XndrM XndrQueue
    bubbleDown idx queue = do
      let
        rightChildIdx   = getRightChild idx
        leftChildIdx    = getLeftChild  idx
        -- FIXME: Again, problematic if this ever isn't true
        parent     = queue ^?! innerQueue . ix idx
        maybeRightChild = queue ^? innerQueue . ix rightChildIdx
        maybeLeftChild  = queue ^? innerQueue . ix leftChildIdx
      case (maybeLeftChild, maybeRightChild) of
        (Nothing, Nothing)
          -> pure queue
        (Nothing, Just rChild)
          -> bool (pure queue) (continueBubble idx rightChildIdx queue)
          =<< compareNodes rChild parent
        (Just lChild, Nothing)
          -> bool (pure queue) (continueBubble idx leftChildIdx queue)
          =<< compareNodes lChild parent
        (Just lChild, Just rChild) -> do
          lrComp <- compareNodes lChild rChild
          if lrComp then do
            lComp <- compareNodes lChild parent
            if lComp then
              continueBubble idx leftChildIdx queue
            else do
              rComp <- compareNodes rChild parent
              if rComp then
                continueBubble idx rightChildIdx queue
              else
                pure queue
          else do
            rComp <- compareNodes rChild parent
            if rComp then
              continueBubble idx rightChildIdx queue
            else
              pure queue

    continueBubble
      :: Int
      -> Int
      -> XndrQueue
      -> ExceptT ReducerError XndrM XndrQueue
    continueBubble parentIdx childIdx queue = do
      queue' <- throwEither $ reduceXndr (DoSwap parentIdx childIdx) queue
      bubbleDown childIdx queue'


mutationInsert :: Topic -> ExceptT ReducerError XndrM XndrQueue
mutationInsert topic = do
  queue <- lift getQueue
  bool (handleInsert queue) (pure queue) . elem topic . view innerQueue $ queue
  where
    lastIndex :: XndrQueue -> Int
    lastIndex q = length (q ^. innerQueue) - 1

    handleInsert :: XndrQueue -> ExceptT ReducerError XndrM XndrQueue
    handleInsert queue = do
      queue' <- throwEither $ reduceXndr (DoAppend topic) queue
      bubbleUp queue' $ lastIndex queue'

    bubbleUp :: XndrQueue -> Int -> ExceptT ReducerError XndrM XndrQueue
    bubbleUp q n
      | n < 0 = error "Improper parent calculation in the XndrQueue"
      | n == 0 = pure q
      | otherwise = do
          let
            parent = getParent n
            -- FIXME: These vals will error if everything is horribly wrong
            -- should property test
            nVal      = q ^?! innerQueue . ix n
            parentVal = q ^?! innerQueue . ix parent
          comp <- compareNodes nVal parentVal
          bool (pure q) (continueBubble n parent q) comp

    continueBubble
      :: Int
      -> Int
      -> XndrQueue
      -> ExceptT ReducerError XndrM XndrQueue
    continueBubble n parent q
      = (`bubbleUp` parent) =<< throwEither (reduceXndr (DoSwap n parent) q)


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

-- | Message indicating the queue is empty
emptyQueueMsg :: XndrM ()
emptyQueueMsg = liftIO $ putStrLn "Nothing in the queue!"

getParent :: Int -> Int
getParent n
  | n > 0     = div (n - 1) 2
  | otherwise = 0

getLeftChild :: Int -> Int
getLeftChild n
  | n < 0     = 0
  | otherwise = 2*n + 1

getRightChild :: Int -> Int
getRightChild n
  | n < 0     = 0
  | otherwise = 2*n + 2

compareNodes :: Topic -> Topic -> ExceptT ReducerError XndrM Bool
compareNodes nVal parentVal = do
  mCompFn <- lift $ view compFn
  maybe (getComp nVal parentVal) (\f -> pure $ f nVal parentVal) mCompFn
  where
    getComp
      :: Topic
      -> Topic
      -> ExceptT ReducerError XndrM Bool
    getComp child parent = do
      liftIO
        . putStrLn
        $ "Is \""
        <> child
        <> "\" higher priority than \""
        <> parent
        <> "\"? (y/n)"
      line <- getLine
      case line of
        "y" -> pure True
        "n" -> pure False
        _   -> getComp child parent

-- | Make the text to display for a tree view
createDisplayTree :: [Topic] -> [Text]
createDisplayTree = reverse . flip execState mempty . doCreate 0 ""
  where
    doCreate :: Int -> Text -> [Text] -> State [Text] ()
    doCreate idx spacer topics
      = maybe (pure ()) processTopic
      $ topics ^? ix idx
      where
        processTopic topic = do
          modify (spacer <> topic :)
          doCreate (getLeftChild idx) ("-" <> spacer) topics
          doCreate (getRightChild idx) ("-" <> spacer) topics
