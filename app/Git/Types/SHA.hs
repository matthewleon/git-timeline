{-# LANGUAGE OverloadedStrings #-}

module Git.Types.SHA (
  SHA
, fromText
, toText
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import           Data.Maybe (Maybe(Just, Nothing))
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Prelude hiding (fail)

newtype SHA = SHA ByteString
  deriving (Eq, Ord, Show)

toText :: SHA -> Text
toText (SHA bs) = decodeUtf8 $ B16.encode bs

fromText :: Text -> Maybe SHA
fromText t = case B16.decode $ encodeUtf8 t of
  (bs, "") -> Just $ SHA bs
  _        -> Nothing
