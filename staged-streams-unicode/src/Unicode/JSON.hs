{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}
-- | JSON string literal decoder
module Unicode.JSON (
    unescapeText,
    UnicodeException (..),
) where

import Control.Exception (Exception, throwIO, try)
import System.IO.Unsafe  (unsafePerformIO)

import Staged.Commons

import qualified Data.ByteString    as BS
import qualified Data.Primitive     as P
import qualified Data.Text.Array    as T
import qualified Data.Text.Internal as T

import Unicode.ByteString.Source
import Unicode.JSON.Decoder
import Unicode.Text.Sink

-- for prettier splice
import Data.Bits          (shiftL, shiftR, (.&.), (.|.))
import Data.Word          (Word16, Word32, Word8)
import Foreign.ForeignPtr (plusForeignPtr, withForeignPtr)
import Foreign.Ptr        (Ptr, plusPtr)
import Foreign.Storable   (peek)

-------------------------------------------------------------------------------
-- Things
-------------------------------------------------------------------------------

unescapeText :: BS.ByteString -> Either UnicodeException T.Text
unescapeText = unsafePerformIO . try . unescapeTextIO

data UnicodeException = UnicodeException deriving (Show)
instance Exception UnicodeException

invalidUtf8 :: IO T.Text
invalidUtf8 = throwIO UnicodeException

-------------------------------------------------------------------------------
-- stream processor
-------------------------------------------------------------------------------

unescapeTextIO :: BS.ByteString -> IO T.Text
unescapeTextIO bs =
    $$(withUnpackedByteString [|| bs ||] $ \len w8s ->
      sinkText [|| invalidUtf8 ||] len [|| () ||] (jsonStringDecoder w8s))
