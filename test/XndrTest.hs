{- HLINT ignore "Reduce duplication" -}
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
--------------------------------------------------------------------------------
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
--------------------------------------------------------------------------------

main :: IO ()
main
  = defaultMain
  . localOption (HedgehogTestLimit $ Just 5)
  $ testGroup "xndr commands"
  [ test_xndrQueueCreate
  , test_xndrQueueDelete
  , test_xndrQueueList
  , test_xndrQueueDesc
  ]


--------------------------------------------------------------------------------

test_xndrQueueCreate :: TestTree
test_xndrQueueCreate
  = testGroup "QueueCreate command"
    [ testProperty "gives a valid response"
        successfulQueueCreate
    , testProperty "successfully creates with no description"
        successfulUndescribedQueueCreate
    , testProperty "successfully creates with a description"
        successfulDescribedQueueCreate
    , testProperty "lets the user know about a conflict"
        conflictQueueCreate
    ]

successfulQueueCreate :: Property
successfulQueueCreate = property $ do
  (createdQueueResponse, queueName) <- lift . withTestDatabase $ do
    queueName
      <- Gen.sample
      . Gen.text (Range.linear 0 10)
      $ Gen.ascii
    createQueueResponse
      <- handleCmd (QueueCreate queueName Nothing)
    pure (createQueueResponse, queueName)

  createdQueueResponse === QueueCreateSuccess queueName

successfulUndescribedQueueCreate :: Property
successfulUndescribedQueueCreate = property $ do
  (queueInfoResponse, queueName) <- lift . withTestDatabase $ do
    queueName
      <- Gen.sample
      . Gen.text (Range.linear 0 10)
      $ Gen.ascii
    handleCmd (QueueCreate queueName Nothing)
    queueInfoResponse <- handleCmd (QueueInfo queueName)
    pure (queueInfoResponse, queueName)

  queueInfoResponse === QueueInfoEmpty queueName

successfulDescribedQueueCreate :: Property
successfulDescribedQueueCreate = property $ do
  (queueInfoResponse, queueName, queueDesc) <- lift . withTestDatabase $ do
    queueName
      <- Gen.sample
      . Gen.text (Range.linear 0 10)
      $ Gen.ascii
    queueDesc
      <- Gen.sample
      . Gen.text (Range.linear 0 10)
      $ Gen.ascii
    handleCmd (QueueCreate queueName $ Just queueDesc)
    queueInfoResponse <- handleCmd (QueueInfo queueName)
    pure (queueInfoResponse, queueName, queueDesc)

  queueInfoResponse === QueueInfoSuccess queueName queueDesc

conflictQueueCreate :: Property
conflictQueueCreate = property $ do
  (createdQueueResponse, queueName) <- lift . withTestDatabase $ do
    queueName
      <- Gen.sample
      . Gen.text (Range.linear 0 10)
      $ Gen.ascii
    handleCmd (QueueCreate queueName Nothing)
    createQueueResponse
      <- handleCmd (QueueCreate queueName Nothing)
    pure (createQueueResponse, queueName)

  createdQueueResponse === QueueCreateExists queueName


--------------------------------------------------------------------------------

test_xndrQueueDelete :: TestTree
test_xndrQueueDelete
  = testGroup "QueueDelete"
    [ testProperty "successfully deletes a queue"
        successfulQueueDelete
    , testProperty "does nothing if queue doesn't exist"
        emptyQueueDelete
    ]

successfulQueueDelete :: Property
successfulQueueDelete = property $ do
  (deleteQueueResponse, queueName) <- lift . withTestDatabase $ do
    queueName
      <- Gen.sample
      . Gen.text (Range.linear 0 10)
      $ Gen.ascii
    handleCmd (QueueCreate queueName Nothing)
    (, queueName) <$> handleCmd (QueueDelete queueName)
  deleteQueueResponse === QueueDeleteSuccess queueName

emptyQueueDelete :: Property
emptyQueueDelete = property $ do
  listQueuesResponse <- lift . withTestDatabase $ do
    queueName
      <- Gen.sample
      . Gen.text (Range.linear 0 10)
      $ Gen.ascii
    handleCmd (QueueDelete queueName)
    handleCmd QueueList
  listQueuesResponse === QueueListResponse []


--------------------------------------------------------------------------------

test_xndrQueueList :: TestTree
test_xndrQueueList
  = testGroup "QueueList"
  [ testProperty "successfully lists a created queue"
      singleQueueQueueList
  , testProperty "returns an empty list when empty"
      noQueuesQueueList
  , testProperty "returns all queues"
      multiQueueQueueList
  ]

singleQueueQueueList :: Property
singleQueueQueueList = property $ do
  (listQueuesResponse, queueName) <- lift . withTestDatabase $ do
    queueName
      <- Gen.sample
      . Gen.text (Range.linear 0 10)
      $ Gen.ascii
    handleCmd (QueueCreate queueName Nothing)
    (, queueName) <$> handleCmd QueueList
  listQueuesResponse === QueueListResponse [queueName]

noQueuesQueueList :: Property
noQueuesQueueList = property $ do
  listQueuesResponse <- lift . withTestDatabase $ handleCmd QueueList
  listQueuesResponse === QueueListResponse []

multiQueueQueueList :: Property
multiQueueQueueList = property $ do
  (listQueuesResponse, queueNames) <- lift . withTestDatabase $ do
    queueNames
      <- Gen.sample
      . genUniqueList (Range.linear 0 10)
      . Gen.text (Range.linear 0 10)
      $ Gen.ascii
    traverse_ (\q -> handleCmd (QueueCreate q Nothing)) queueNames
    (,queueNames) <$> handleCmd QueueList
  compContents listQueuesResponse queueNames
  where
    compContents (QueueListResponse responseNames) trueNames =
      sort responseNames === sort trueNames
    compContents _ _ = failure


--------------------------------------------------------------------------------

test_xndrQueueDesc :: TestTree
test_xndrQueueDesc
  = testGroup "QueueDesc"
  [ testProperty "returns the correct response"
      successfulQueueDesc
  , testProperty "returns the correct response"
      infoQueueDesc
  , testProperty "says when the queue doesn't exist"
      nonexistentQueueDesc
  ]

successfulQueueDesc :: Property
successfulQueueDesc = property $ do
  (descResponse, queueName) <- lift . withTestDatabase $ do
    queueName
      <- Gen.sample
      . Gen.text (Range.linear 0 10)
      $ Gen.ascii
    queueDesc
      <- Gen.sample
      . Gen.text (Range.linear 0 10)
      $ Gen.ascii
    handleCmd (QueueCreate queueName Nothing)
    (, queueName) <$> handleCmd (QueueDesc   queueName queueDesc)
  descResponse === QueueDescSuccess queueName

infoQueueDesc :: Property
infoQueueDesc = property $ do
  (infoResponse, queueName, queueDesc) <- lift . withTestDatabase $ do
    queueName
      <- Gen.sample
      . Gen.text (Range.linear 0 10)
      $ Gen.ascii
    queueDesc
      <- Gen.sample
      . Gen.text (Range.linear 0 10)
      $ Gen.ascii
    handleCmd (QueueCreate queueName Nothing)
    handleCmd (QueueDesc   queueName queueDesc)
    (, queueName, queueDesc) <$> handleCmd (QueueInfo   queueName)
  infoResponse === QueueInfoSuccess queueName queueDesc

nonexistentQueueDesc :: Property
nonexistentQueueDesc = property $ do
  (descResponse, queueName) <- lift . withTestDatabase $ do
    queueName
      <- Gen.sample
      . Gen.text (Range.linear 0 10)
      $ Gen.ascii
    queueDesc
      <- Gen.sample
      . Gen.text (Range.linear 0 10)
      $ Gen.ascii
    (, queueName) <$> handleCmd (QueueDesc queueName queueDesc)
  descResponse === QueueDescMissing queueName
