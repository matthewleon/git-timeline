module Git.Log (gitLogOneline) where

import qualified Data.Text as T
import Git.Command (Subcommand(..), Flag(..), git)
import Turtle

gitLogOneline :: Shell Line
gitLogOneline = gitLog [Oneline]

data LogFlag = Oneline
  deriving (Show)

logFlagToFlag :: LogFlag -> Flag
logFlagToFlag = Flag . T.pack . show

gitLog :: [LogFlag] -> Shell Line
gitLog = git Log . fmap logFlagToFlag
