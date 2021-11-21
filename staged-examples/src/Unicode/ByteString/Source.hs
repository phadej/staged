{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Unicode.ByteString.Source (
    withUnpackedByteString,
) where

import Data.Word          (Word8)
import Foreign.ForeignPtr (plusForeignPtr, withForeignPtr)
import Foreign.Ptr        (Ptr, plusPtr)
import Foreign.Storable   (peek)

import Staged.Commons

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS
import qualified Staged.Stream.Fallible   as S

withUnpackedByteString :: C BS.ByteString -> (S.StreamFM IO () Word8 -> C (IO r)) -> C (IO r)
withUnpackedByteString bs k =
    [|| case $$bs of
         BS.PS fptr off len -> withForeignPtr (plusForeignPtr fptr off) $ \begin -> do
            let end = plusPtr begin len
            $$(k (foreachPtr [|| begin ||] [|| end ||]))
     ||]

foreachPtr :: C (Ptr Word8) -> C (Ptr Word8) -> S.StreamFM IO () Word8
foreachPtr begin end = S.mkStreamFM start step where
    start :: C () -> C (Ptr Word8)
    start _ = begin

    step :: C (Ptr Word8) -> (S.FallibleStep (C Word8) (C (Ptr Word8)) -> C (IO r)) -> C (IO r)
    step ptr k = sIfThenElse
        [|| $$ptr /= $$end ||]
        ([|| peek ($$ptr :: Ptr Word8) ||] >>>= \w8 -> k $ S.Emit w8 [|| plusPtr $$ptr 1 ||])
        (k S.Stop)
