module Spec where

import Discover
import System.Environment (withArgs)

main :: IO ()
main = do
  args <- getArgs
  withArgs
    ("--color=always" : args)
    Discover.main
