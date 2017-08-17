module Main where

import Git (gitLogOneline)
import Turtle

main :: IO ()
main = view gitLogOneline
