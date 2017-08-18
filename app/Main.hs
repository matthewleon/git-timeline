module Main where

import Git.Diff (diffStat)
import Turtle

main :: IO ()
main = view diffStat
