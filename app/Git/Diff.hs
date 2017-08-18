{-# LANGUAGE OverloadedStrings #-}

module Git.Diff (diffStat) where

import Control.Exception.Safe (throwString)
import qualified Data.Text as T
import Git.Command (Subcommand(Diff), Flag(..), git)
import Turtle
import Prelude hiding (FilePath)

data DiffStat = DiffStat {
  additions :: Integer
, deletions :: Integer
, filepath  :: FilePath
}
  deriving (Show)

diffStat :: Shell DiffStat
diffStat = parseLine =<< lineToText <$> diffStat'
  where
  parseLine t = case match statPattern t of
    [(add, sub, fp)] -> return $ DiffStat add sub fp
    _ -> liftIO . throwString
         $ "Failed to parse diffstat line: " <> T.unpack t
    where
    statPattern = (,,) <$> (decimal <* spaces1)
                       <*> (decimal <* spaces1)
                       <*> (fromText <$> chars)

diffStat' :: Shell Line
diffStat' = gitDiff [Numstat]

data DiffFlag = Numstat
  deriving (Show)

diffFlagToFlag :: DiffFlag -> Flag
diffFlagToFlag = Flag . T.pack . show

gitDiff :: [DiffFlag] -> Shell Line
gitDiff = git Diff . fmap diffFlagToFlag
