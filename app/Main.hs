--------------------------------------------------------------------------------
-- |
-- Module: Main
-- Description: The command line executable for xndr.
-- Maintainers: Cameron Kingsbury <cameron@urbint.com>
-- Maturity: Draft
--
--
-- = Detailed Description
-- Provides a simple CLI for xndr that allows for seeing the most important
-- stored item.
--
--
-- = Usage Example
-- @xndr top@ shows the highest priority stored item
--
--------------------------------------------------------------------------------
module Main
  ( module Main
  ) where
--------------------------------------------------------------------------------
import Prelude
--------------------------------------------------------------------------------
import Options.Applicative
--------------------------------------------------------------------------------
import Xndr
  ( xndr
  , XndrCmd(..)
  )
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Executable

-- | Takes command line arguments, and executes the program
main :: IO ()
main = xndr =<< customExecParser p opts
  where
    opts = info (parseXndrCmd <**> helper) desc
    p    = prefs showHelpOnEmpty
    desc = fullDesc
         <> progDesc "A priority queue for all of the tasks \
                    \you would like to keep track of by priority. \
                    \Will expand to include concrete use cases soon."
         <> header "Xndr - A priority queue for humans"

--------------------------------------------------------------------------------
-- Parser

-- | Parser for `XndrCmd`s
parseXndrCmd :: Parser XndrCmd
parseXndrCmd
  = hsubparser
  ( command "q"
    ( info
      parseQueueCommand
      (progDesc "Actions to be taken on queues")
    )
  )

parseQueueCommand :: Parser XndrCmd
parseQueueCommand
  = hsubparser
  ( command "ls"
    ( info
      (pure QueueList)
      (progDesc "List all priority queues")
    )
  <> command "add"
    ( info
      parseQueueCreate
      (progDesc "Make a priority queue")
    )
  <> command "rm"
    ( info
      parseQueueDelete
      (progDesc "Delete a priority queue")
    )
  <> command "info"
    ( info
      parseQueueInfo
      (progDesc "Get info on a priority queue")
    )
  <> command "desc"
    ( info
      parseQueueDesc
      (progDesc "Provide description for a priority queue")
    )
  )

-- | Parser for the `QueueCreate` command
parseQueueCreate :: Parser XndrCmd
parseQueueCreate
  =   QueueCreate
  <$> argument str (metavar "QUEUE")
  <*> optional (argument str (metavar "DESCRIPTION"))

-- | Parser for the `QueueDelete` command
parseQueueDelete :: Parser XndrCmd
parseQueueDelete
  =   QueueDelete
  <$> argument str (metavar "QUEUE")

-- | Parser for the `QueueInfo` command
parseQueueInfo :: Parser XndrCmd
parseQueueInfo
  =   QueueInfo
  <$> argument str (metavar "QUEUE")

-- | Parser for the `QueueInfo` command
parseQueueDesc :: Parser XndrCmd
parseQueueDesc
  =   QueueDesc
  <$> argument str (metavar "QUEUE")
  <*> argument str (metavar "DESCRIPTION")
