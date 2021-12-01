{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module Unicode (
    fromUTF8BS,
    textFromUTF8BS,
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
import Unicode.PrimArray.Sink
import Unicode.UTF16.Encoder
import Unicode.UTF8.Encoder
import Unicode.UTF8.Decoder

-- for prettier splice
{-
import Data.Bits          (shiftL, (.&.), (.|.))
import Data.Word          (Word8, Word16)
import Foreign.ForeignPtr (plusForeignPtr, withForeignPtr)
import Foreign.Ptr        (Ptr, plusPtr)
import Foreign.Storable   (peek)
-}

fromUTF8BS :: BS.ByteString -> String
fromUTF8BS bs = unsafePerformIO $
    $$(withUnpackedByteString [|| bs ||] $ \_len w8s ->
          S.toList [|| invalidUtf8 ||] [|| () ||]
        $ S.map [|| \x -> chr (fromIntegral (x :: Word32)) ||]
        $ utf8decoder w8s)
  where
    invalidUtf8 :: IO String
    invalidUtf8 = fail "Invalid UTF8"

textFromUTF8BS :: BS.ByteString -> T.Text
textFromUTF8BS bs = unsafePerformIO $
    $$(withUnpackedByteString [|| bs ||] $ \len w8s ->
#if MIN_VERSION_text(2,0,0)
       sinkPrimArray [|| invalidUtf8 ||] len [|| () ||] (utf8encoder $ utf8decoder w8s) $ \len' p ->
       sreturn [|| case $$p of P.PrimArray ba -> T.Text (T.ByteArray ba) 0 $$len' ||])
#else
       sinkPrimArray [|| invalidUtf8 ||] len [|| () ||] (utf16encoder $ utf8decoder w8s) $ \len' p ->
       sreturn [|| case $$p of P.PrimArray ba -> T.Text (T.Array ba) 0 $$len' ||])
#endif
  where
    invalidUtf8 :: IO T.Text
    invalidUtf8 = fail "Invalid UTF8"
