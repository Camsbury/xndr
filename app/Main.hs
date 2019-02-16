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
import Xndr (xndr, XndrCmd(..))
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
                    \ you would like to keep track of in a \
                    \ priority queue. Will expand to include \
                    \ concrete use cases soon."
         <> header "Xndr - A priority queue for humans"

--------------------------------------------------------------------------------
-- Parser

-- | Parser for `XndrCmd`s
parseXndrCmd :: Parser XndrCmd
parseXndrCmd
  = hsubparser
  ( command "top"
    ( info
      (pure Top)
      (progDesc "Print highest priority item in queue")
    )
  <> command "pop"
    ( info
      (pure Pop)
      (progDesc "Pop highest priority item from queue")
    )
  <> command "list"
    ( info
      (pure List)
      (progDesc "List the items in the queue")
    )
  <> command "delete"
    ( info
      parseDelete
      (progDesc "Delete a topic from the queue")
    )
  <> command "insert"
    ( info
      parseInsert
      (progDesc "Insert a topic into the queue")
    )
  <> command "info"
    ( info
      parseInfo
      (progDesc "Get the description of a topic")
    )
  <> command "describe"
    ( info
      parseDescribe
      (progDesc "Write the description of a topic")
    )
  )

-- | Parser for the `Delete` command
parseDelete :: Parser XndrCmd
parseDelete
  =   Delete
  <$> argument str (metavar "TOPIC")

-- | Parser for the `Insert` command
parseInsert :: Parser XndrCmd
parseInsert
  =   Insert
  <$> argument str (metavar "TOPIC")

-- | Parser for the `Info` command
parseInfo :: Parser XndrCmd
parseInfo
  =   Info
  <$> argument str (metavar "TOPIC")

-- | Parser for the `Describe` command
parseDescribe :: Parser XndrCmd
parseDescribe
  =   Describe
  <$> argument str (metavar "TOPIC")
  <*> argument str (metavar "DESCRIPTION")
