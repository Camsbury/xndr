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
import Xndr (xndr)
--------------------------------------------------------------------------------

-- | Takes command line arguments, and executes the program
main :: IO ()
main = xndr =<< getArgs
