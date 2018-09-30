--------------------------------------------------------------------------------
module XndrTest where
--------------------------------------------------------------------------------
import Test.Prelude
--------------------------------------------------------------------------------
import Xndr
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
  $ xndrResponse mempty Top === "Nothing in the queue!"

singletonQueueTopResponse :: Property
singletonQueueTopResponse = property $ do
  topic <- Gen.sample $ Gen.text (Range.linear 0 10) Gen.ascii
  xndrResponse (XndrQueue [topic]) Top
    === "\"" <> topic <> "\" is the most important topic in the queue."

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
  xndrResponse (XndrQueue [topicHigher, topicLower]) Top
    === "\"" <> topicHigher <> "\" is the most important topic in the queue."

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
    queue = reduceXndr (DoInsert topic) mempty
  xndrResponse queue Top
    === "\"" <> topic <> "\" is the most important topic in the queue."
