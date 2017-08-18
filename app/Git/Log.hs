{-# LANGUAGE OverloadedStrings #-}

module Git.Log (logOneline) where

import Control.Arrow ((>>>))
import Control.Exception.Safe (throwString)
import Data.Maybe (maybe)
import qualified Data.Text as T
import Git.Command (Subcommand(..), Flag(..), git)
import Git.Types.SHA (SHA)
import qualified Git.Types.SHA as SHA
import Turtle

logOneline :: Shell (SHA, Text)
logOneline = parseLine =<< logOneline'
  where
  parseLine line = case match logPattern t of
    [(Just sha, comment)] -> pure (sha, comment)
    _ -> liftIO . throwString
         $ "Failed to parse log line: " <> T.unpack t
    where
    t = lineToText line
    logPattern = (,) <$> (sha <* char ' ') <*> chars
      where sha = SHA.fromText <$> plus hexDigit

logOneline' :: Shell Line
logOneline' = gitLog [Oneline]

data LogFlag = Oneline
  deriving (Show)

logFlagToFlag :: LogFlag -> Flag
logFlagToFlag Oneline = Flag "pretty=oneline"

gitLog :: [LogFlag] -> Shell Line
gitLog = git Log . fmap logFlagToFlag
