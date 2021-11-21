{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Unicode (
    fromUTF8BS,
) where

import Data.Char        (chr)
import Data.Word        (Word32)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString        as BS
import qualified Staged.Stream.Fallible as S

import Unicode.ByteString.Source
import Unicode.UTF8.Decoder

-- for prettier splice
import Data.Bits          (shiftL, (.&.), (.|.))
import Data.Word          (Word8)
import Foreign.ForeignPtr (plusForeignPtr, withForeignPtr)
import Foreign.Ptr        (Ptr, plusPtr)
import Foreign.Storable   (peek)

fromUTF8BS :: BS.ByteString -> String
fromUTF8BS bs = unsafePerformIO $
    $$(withUnpackedByteString [|| bs ||] $ \w8s ->
          S.toList [|| invalidUtf8 ||] [|| () ||]
        $ S.map [|| \x -> chr (fromIntegral (x :: Word32)) ||]
        $ utf8decoder w8s)
  where
    invalidUtf8 :: IO String
    invalidUtf8 = fail "Invalid UTF8"
