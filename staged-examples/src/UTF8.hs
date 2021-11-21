{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module UTF8 where

import Data.Char       (chr)
import Data.Word       (Word32)

import qualified Data.ByteString      as BS
import qualified Staged.Stream.Pure   as S

import UTF8.Stream

-- for prettier splice
import Data.Bits (shiftL, (.&.), (.|.))

toString :: BS.ByteString -> String
toString bs =
    $$(S.toList [|| bs ||]
        $ S.map [|| \x -> chr (fromIntegral (x :: Word32)) ||]
        $ decoder
        $ S.fromList [|| BS.unpack ||])
