module Main where

import Git.Log (gitLogOneline)
import Turtle

main :: IO ()
main = view gitLogOneline
