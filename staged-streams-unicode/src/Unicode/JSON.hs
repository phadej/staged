{-# LANGUAGE TemplateHaskell #-}
-- | JSON string literal decoder
module Unicode.JSON (
    unescapeString,
) where

import Data.Char        (chr)
import Data.Word        (Word32)
import System.IO.Unsafe (unsafePerformIO)

import Staged.Commons

import qualified Data.ByteString        as BS
import qualified Data.Primitive         as P
import qualified Data.Text.Array        as T
import qualified Data.Text.Internal     as T
import qualified Staged.Stream.Fallible as S

import Unicode.ByteString.Source
import Unicode.JSON.Decoder
import Unicode.PrimArray.Sink
import Unicode.UTF16.Encoder

-- for prettier splice
{-
import Data.Bits          (shiftL, (.&.), (.|.))
import Data.Word          (Word8, Word16)
import Foreign.ForeignPtr (plusForeignPtr, withForeignPtr)
import Foreign.Ptr        (Ptr, plusPtr)
import Foreign.Storable   (peek)
-}

unescapeString :: BS.ByteString -> String
unescapeString bs = unsafePerformIO $
    $$(withUnpackedByteString [|| bs ||] $ \_len w8s ->
          S.toList [|| invalidUtf8 ||] [|| () ||]
        $ S.map [|| \x -> chr (fromIntegral (x :: Word32)) ||]
        $ jsonStringDecoder w8s)
  where
    invalidUtf8 :: IO String
    invalidUtf8 = fail "Invalid UTF8 or JSON escape sequence"


