{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Unicode (
    fromUTF8BS,
) where

import Data.Char             (chr)
import Data.Word             (Word32)

import qualified Data.ByteString        as BS
import qualified Staged.Stream.Fallible as S

import Unicode.UTF8.Decoder
import Unicode.Types

-- for prettier splice
import Data.Bits (shiftL, (.&.), (.|.))

fromUTF8BS :: BS.ByteString -> String
fromUTF8BS bs = unI' $
    $$(S.toList [|| invalidUtf8 ||] [|| bs ||]
        $ S.map [|| \x -> chr (fromIntegral (x :: Word32)) ||]
        $ utf8decoder
        $ S.fromList [|| BS.unpack ||])
  where
    invalidUtf8 :: I' String
    invalidUtf8 = error "Invalid UTF8"
