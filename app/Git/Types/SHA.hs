{-# LANGUAGE OverloadedStrings #-}

module Git.Types.SHA (
  SHA
, fromText
, toText
) where

import           Control.Monad.Fail (MonadFail(fail))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Prelude hiding (fail)

newtype SHA = SHA ByteString
  deriving (Eq, Ord, Read)

toText :: SHA -> Text
toText (SHA bs) = decodeUtf8 $ B16.encode bs

fromText :: MonadFail m => Text -> m SHA
fromText t =
    case B16.decode $ encodeUtf8 t of
        (bs, "") -> return $ SHA bs
        _        -> fail "Invalid base16 encoding"
