{-# LANGUAGE OverloadedStrings #-}

module Git.Command (
  Subcommand(..)
, Flag(..)
, git
) where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Turtle
import Prelude hiding (FilePath)

data Subcommand = Log
  deriving (Show)

lowerTextShow :: Show a => a -> Text
lowerTextShow = T.toLower . T.pack . show

newtype Flag = Flag Text

formatFlag :: Flag -> Text
formatFlag (Flag f) = "--" <> T.toLower f

git :: Subcommand -> [Flag] -> Shell Line
git subcommand flags = inproc "git" params empty
  where params = lowerTextShow subcommand : (formatFlag <$> flags)
