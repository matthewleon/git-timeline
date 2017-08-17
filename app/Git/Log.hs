{-# LANGUAGE OverloadedStrings #-}

module Git.Log (gitLogOneline) where

import Control.Arrow ((>>>))
import Control.Exception.Safe (throwString)
import Data.Maybe (maybe)
import qualified Data.Text as T
import Git.Command (Subcommand(..), Flag(..), git)
import Git.Types.SHA (SHA)
import qualified Git.Types.SHA as SHA
import Turtle

gitLogOneline :: Shell (SHA, Text)
gitLogOneline = parseLine =<< gitLogOneline'
  where
  parseLine :: Line -> Shell (SHA, Text)
  parseLine =
        lineToText
    >>> \t ->
      case match logPattern t of
        [(shaText, comment)] ->
          maybe
          (throwText $ "Failed to parse SHA: " <> shaText)
          (\sha -> return (sha, comment))
          $ SHA.fromText shaText
        _ -> throwText $ "Failed to parse log line: " <> t
    where
    logPattern = (,) <$> (sha <* char ' ') <*> chars
      where sha = plus hexDigit
    throwText = liftIO . throwString . T.unpack

gitLogOneline' :: Shell Line
gitLogOneline' = gitLog [Oneline]

data LogFlag = Oneline
  deriving (Show)

logFlagToFlag :: LogFlag -> Flag
logFlagToFlag Oneline = Flag "pretty=oneline"

gitLog :: [LogFlag] -> Shell Line
gitLog = git Log . fmap logFlagToFlag
