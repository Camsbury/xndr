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
import Control.Lens (ix)
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
    _compTable
      = mempty
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
    . Gen.text (Range.linear 0 10)
    $ Gen.ascii
  topicHigher
    <- Gen.sample
    . Gen.filter (/= topicLower)
    . Gen.text (Range.linear 0 10)
    $ Gen.ascii

  let
    _queueFileName
      = mkTestFileName testState
    _queueDir
      = "/tmp/"
    _compTable
      = Just $ mapFromList
        [ (topicMiddle, mapFromList [(topicHigher, False)])
        , (topicHigher, mapFromList [(topicLower, True)])
        ]
  _queueRef <- newIORef mempty

  topTopic <- liftIO . (`runReaderT` Env{..}) $ do
    queue <- getQueue
    handleCmd (Insert topicLower)
    handleCmd (Insert topicHigher)
    handleCmd (Insert topicMiddle)

    queryTop <$> getQueue

  liftIO $ removeFile (_queueDir <> _queueFileName)

  topTopic === Just topicHigher
