{-# LANGUAGE OverloadedStrings #-}

module Git.Log (logOneline) where

import Control.Arrow ((>>>))
import Control.Exception.Safe (throwString)
import Data.Maybe (maybe)
import qualified Data.Text as T
import Git.Command (Subcommand(Log), Flag(..), git)
import Git.Types.SHA (SHA)
import qualified Git.Types.SHA as SHA
import Turtle

logOneline :: Shell (SHA, Text)
logOneline = parseLine =<< lineToText <$> logOneline'
  where
  parseLine t = case match logPattern t of
    [(Just sha, comment)] -> pure (sha, comment)
    _ -> liftIO . throwString
         $ "Failed to parse log line: " <> T.unpack t
    where
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
