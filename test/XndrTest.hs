--------------------------------------------------------------------------------
module XndrTest where
--------------------------------------------------------------------------------
import Test.Prelude
import Test.Support
--------------------------------------------------------------------------------
import Xndr
--------------------------------------------------------------------------------
import Control.Lens.Operators
--------------------------------------------------------------------------------
import Control.Lens (ix, non)
import System.Directory (removeFile)
--------------------------------------------------------------------------------
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
--------------------------------------------------------------------------------

main :: IO ()
main
  = defaultMain
  . localOption (HedgehogTestLimit $ Just 5)
  $ testGroup "xndr commands"
    [ test_xndrTop
    , test_xndrInsert
    , test_readWrite
    , test_prioritizeInsert
    , test_prioritizeDelete
    , test_prioritizePop
    ]

--------------------------------------------------------------------------------
-- Top

test_xndrTop :: TestTree
test_xndrTop
  = testGroup "top command"
    [ testProperty "returns nothing with an empty queue"
        emptyQueueTopResponse
    , testProperty "returns the only item in a one-item queue"
        singletonQueueTopResponse
    , testProperty "returns the top item in a two-item queue"
        twoMemberQueueTopResponse
    ]

emptyQueueTopResponse :: Property
emptyQueueTopResponse
  = property
  $ queryTop mempty === Nothing

singletonQueueTopResponse :: Property
singletonQueueTopResponse = property $ do
  topic <- Gen.sample $ Gen.text (Range.linear 0 10) Gen.ascii
  queryTop (XndrQueue [topic])
    === Just topic

twoMemberQueueTopResponse :: Property
twoMemberQueueTopResponse = property $ do
  topicLower
    <- Gen.sample
    . Gen.text (Range.linear 0 10)
    $ Gen.ascii
  topicHigher
    <- Gen.sample
    . Gen.filter (/= topicLower)
    . Gen.text (Range.linear 0 10)
    $ Gen.ascii
  queryTop (XndrQueue [topicHigher, topicLower]) === Just topicHigher


--------------------------------------------------------------------------------
-- Insert

test_xndrInsert :: TestTree
test_xndrInsert
  = testGroup "insert command"
    [ testProperty "sucessfully puts something in the queue"
        successfulSingleInsert
    ]

successfulSingleInsert :: Property
successfulSingleInsert = property $ do
  topic <- Gen.sample $ Gen.text (Range.linear 0 10) Gen.ascii
  let
    queue = unsafeEither $ reduceXndr (DoAppend topic) mempty
  queryTop queue
    === Just topic


--------------------------------------------------------------------------------
-- Read and Write

test_readWrite :: TestTree
test_readWrite
  = testGroup "readQueueFile and writeQueueFile"
    [ testProperty "round trips through a write and read" roundTripReadWrite
    ]

roundTripReadWrite :: Property
roundTripReadWrite = property $ do
  testState <- newTestState
  let
    _queueFileName
      = mkTestFileName testState
    _queueDir
      = "/tmp/"
    _compFn
      = Nothing
  _queueRef <- newIORef mempty

  -- Starts off empty
  queue1 <- liftIO . (`runReaderT` Env{..}) $ do
    readQueueFile

    getQueue

  queue1 === mempty

  -- Insert a value
  topic <- Gen.sample $ Gen.text (Range.linear 0 10) Gen.ascii
  liftIO . (`runReaderT` Env{..}) $ do
    modifyQueue . fmap unsafeEither $ reduceXndr (DoAppend topic)
    writeQueueFile

  -- Has inserted value
  queue3 <- liftIO . (`runReaderT` Env{..}) $ do
    readQueueFile
    getQueue

  liftIO $ removeFile (_queueDir <> _queueFileName)

  queue3 === XndrQueue [topic]


--------------------------------------------------------------------------------
-- Insert Prioritization

test_prioritizeInsert :: TestTree
test_prioritizeInsert
  = testGroup "Insert"
    [ testProperty "should prioritize properly" prioritizedInsert
    ]

prioritizedInsert :: Property
prioritizedInsert = property $ do
  testState <- newTestState

  topicLower
    <- Gen.sample
    . Gen.text (Range.linear 0 10)
    $ Gen.ascii
  topicMiddle
    <- Gen.sample
    . Gen.filter (/= topicLower)
    . Gen.text (Range.linear 0 10)
    $ Gen.ascii
  topicHigher
    <- Gen.sample
    . Gen.filter (\x -> x /= topicLower && x /= topicMiddle)
    . Gen.filter (/= topicLower)
    . Gen.text (Range.linear 0 10)
    $ Gen.ascii

  let
    _queueFileName
      = mkTestFileName testState
    _queueDir
      = "/tmp/"
    compFn nVal pVal
      | nVal == topicHigher && pVal == topicLower = True
      | nVal == topicHigher && pVal == topicMiddle = True
      | nVal == topicMiddle && pVal == topicHigher = False
      | nVal == topicMiddle && pVal == topicLower = True
      | nVal == topicLower && pVal == topicHigher = False
      | nVal == topicLower && pVal == topicMiddle = False
      | otherwise = False
    _compFn = Just compFn
  _queueRef <- newIORef mempty

  topTopic <- liftIO . (`runReaderT` Env{..}) $ do
    queue <- getQueue
    handleCmd (Insert topicLower)
    handleCmd (Insert topicHigher)
    handleCmd (Insert topicMiddle)
    queryTop <$> getQueue

  liftIO $ removeFile (_queueDir <> _queueFileName)

  topTopic === Just topicHigher


--------------------------------------------------------------------------------
-- Delete Prioritization

test_prioritizeDelete :: TestTree
test_prioritizeDelete
  = testGroup "Delete"
  [ testProperty "should prioritize properly" prioritizedDelete
  ]

prioritizedDelete :: Property
prioritizedDelete = property $ do
  testState <- newTestState

  topicLower
    <- Gen.sample
    . Gen.text (Range.linear 0 10)
    $ Gen.ascii
  topicMiddle
    <- Gen.sample
    . Gen.filter (/= topicLower)
    . Gen.text (Range.linear 0 10)
    $ Gen.ascii
  topicHigher
    <- Gen.sample
    . Gen.filter (/= topicMiddle)
    . Gen.filter (/= topicLower)
    . Gen.text (Range.linear 0 10)
    $ Gen.ascii

  let
    _queueFileName
      = mkTestFileName testState
    _queueDir
      = "/tmp/"
    compFn nVal pVal
      | nVal == topicHigher && pVal == topicLower = True
      | nVal == topicHigher && pVal == topicMiddle = True
      | nVal == topicMiddle && pVal == topicHigher = False
      | nVal == topicMiddle && pVal == topicLower = True
      | nVal == topicLower  && pVal == topicHigher = False
      | nVal == topicLower  && pVal == topicMiddle = False
      | otherwise = False
    _compFn = Just compFn
  _queueRef
    <- newIORef
    $ XndrQueue
    [ topicHigher
    , topicMiddle
    , topicLower
    ]

  topTopic <- liftIO . (`runReaderT` Env{..}) $ do
    queue <- getQueue
    handleCmd (Delete topicHigher)
    queryTop <$> getQueue

  liftIO $ removeFile (_queueDir <> _queueFileName)

  topTopic === Just topicMiddle


--------------------------------------------------------------------------------
-- Pop Prioritization

test_prioritizePop :: TestTree
test_prioritizePop
  = testGroup "Pop"
  [ testProperty "should prioritize properly" prioritizedPop
  ]

prioritizedPop :: Property
prioritizedPop = property $ do
  testState <- newTestState

  topicLower
    <- Gen.sample
    . Gen.text (Range.linear 0 10)
    $ Gen.ascii
  topicMiddle
    <- Gen.sample
    . Gen.filter (/= topicLower)
    . Gen.text (Range.linear 0 10)
    $ Gen.ascii
  topicHigher
    <- Gen.sample
    . Gen.filter (/= topicMiddle)
    . Gen.filter (/= topicLower)
    . Gen.text (Range.linear 0 10)
    $ Gen.ascii

  let
    _queueFileName
      = mkTestFileName testState
    _queueDir
      = "/tmp/"
    compFn nVal pVal
      | nVal == topicHigher && pVal == topicLower = True
      | nVal == topicHigher && pVal == topicMiddle = True
      | nVal == topicMiddle && pVal == topicHigher = False
      | nVal == topicMiddle && pVal == topicLower = True
      | nVal == topicLower  && pVal == topicHigher = False
      | nVal == topicLower  && pVal == topicMiddle = False
      | otherwise = False
    _compFn = Just compFn
  _queueRef
    <- newIORef
    $ XndrQueue
    [ topicHigher
    , topicMiddle
    , topicLower
    ]

  topTopic <- liftIO . (`runReaderT` Env{..}) $ do
    queue <- getQueue
    handleCmd Pop
    queryTop <$> getQueue

  liftIO $ removeFile (_queueDir <> _queueFileName)

  topTopic === Just topicMiddle
