module Main where

import Git.Log (logOneline)
import Turtle

main :: IO ()
main = view logOneline
