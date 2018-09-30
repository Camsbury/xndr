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
--------------------------------------------------------------------------------
import Control.Lens (ix, makeFieldsNoPrefix)
--------------------------------------------------------------------------------
-- Types
-- TODO: clean these up to be derived from 'CmdTag'

-- | Beginning representation of the Xndr priority queue
newtype XndrQueue
  = XndrQueue
  { _innerQueue :: Vector Text
  }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

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
  = DoPop
  | DoInsert Topic
  | DoDescribe Topic Text
  deriving stock (Eq, Show)


--------------------------------------------------------------------------------
-- Functions

-- | Takes commmand line arguments, and performs the corresponding xndr action.
xndr :: [Text] -> IO ()
xndr rawCmd = do
  queue <- getQueue
  maybe handleNothing (putStrLn . xndrResponse queue) . parseCmd $ rawCmd
  where
    handleNothing
      = print . intercalate " "
      $ "Invalid argument. Valid arguments include:" : cmdList

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

      otherwise
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

-- | Get the queue from the environment
getQueue :: IO XndrQueue
getQueue = pure mempty


-- | The response function for each command
xndrResponse :: XndrQueue -> XndrCmd -> Text
xndrResponse queue
  = \case
      Top
        -> maybe noTop topText
        $ renderCmd TopTag queue

      Pop
        -> "Popped \"topic1\" from the queue."

      Insert topic
        -> "\"" <> topic <> "\" was inserted into the queue."

      Info topic
        -> "\"" <> topic <> "\": descriptions of the topic go here!"

      Describe topic description
        -> "\"" <> topic <> "\" description added"
 where
   topText x = "\"" <> x <> "\" is the most important topic in the queue."
   noTop     = "Nothing in the queue!"


-- | Render a command with the queue
renderCmd :: CmdTag -> XndrQueue -> Maybe Text
renderCmd TopTag queue
  = queue ^? innerQueue . ix 0


--------------------------------------------------------------------------------
-- Reducer

-- | Reduce a 'XndrAction' into the 'XndrQueue'
reduceXndr :: XndrAction -> XndrQueue -> XndrQueue
reduceXndr action queue
  = case action of
      DoPop
        -> queue

      DoInsert topic
        -> queue & innerQueue %~ cons topic

      DoDescribe topic desc
        -> queue
