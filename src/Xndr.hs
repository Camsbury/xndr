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


-- | Takes commmand line arguments, and performs the corresponding xndr action.
xndr :: [Text] -> IO ()
xndr rawCmd = maybe handleNothing (putStrLn . xndrResponse) . parseCmd $ rawCmd
  where
    handleNothing
      = print . intercalate " "
      $ "Invalid argument. Valid arguments include:" : cmdList


-- | The response function for each command
xndrResponse :: XndrCmd -> Text
xndrResponse
  = \case
      Top
        -> "\"topic1\" is the most important topic in the queue."

      Pop
        -> "Popped \"topic1\" from the queue."

      Insert topic
        -> "\"" <> topic <> "\" was inserted into the queue."

      Info topic
        -> "\"" <> topic <> "\": descriptions of the topic go here!"

      Describe topic description
        -> "\"" <> topic <> "\" description added"
