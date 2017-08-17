{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Git (gitLogOneline) where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import TextShow (TextShow(showbPrec, showt))
import TextShow.Generic (genericShowbPrec)
import Turtle
import Prelude hiding (FilePath)

gitLogOneline :: Shell Line
gitLogOneline = gitLog [Oneline]

data Subcommand = Log
  deriving (Show, Generic)

instance TextShow Subcommand where
  showbPrec = genericShowbPrec

lowerTextShow :: TextShow a => a -> Text
lowerTextShow = T.toLower . showt

data LogFlag = Oneline
  deriving (Show, Generic)

instance TextShow LogFlag where
  showbPrec = genericShowbPrec

formatFlag :: TextShow a => a -> Text
formatFlag x = "--" <> lowerTextShow x

gitLog :: [LogFlag] -> Shell Line
gitLog = git Log . fmap formatFlag

git :: Subcommand -> [Text] -> Shell Line
git subcommand flags = inproc "git" (lowerTextShow subcommand : flags) empty
