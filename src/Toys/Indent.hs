module Indent where

import Control.Error.Script
import Control.Error.Safe
import System.Environment

main = runScript $ do
  args <- scriptIO getArgs
  f <- tryHead "Please supply file to indent as argument" args
  str <- scriptIO $ readFile f
  scriptIO $ putStr (indent str)
  where
    indent = unlines . (map (take 4 (repeat ' ') ++)) . lines
