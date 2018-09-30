--------------------------------------------------------------------------------
module XndrTest where
--------------------------------------------------------------------------------
import Test.Prelude
--------------------------------------------------------------------------------
import Xndr
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test_Xndr

test_Xndr :: TestTree
test_Xndr
  = localOption (HedgehogTestLimit $ Just 5)
  $ testGroup "top command"
    [ testProperty "returns nothing with an empty queue" testTopNoElem
    , testProperty "returns the only item in a one-item queue" testTopOneElem
    ]

testTopNoElem :: Property
testTopNoElem = undefined

testTopOneElem :: Property
testTopOneElem = undefined
