{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Unicode.Text.Sink (
    sinkText,
) where

import Staged.Commons
import Data.Word

import qualified Data.ByteString    as BS
import qualified Data.Primitive     as P
import qualified Data.Text.Array    as T
import qualified Data.Text.Internal as T
import qualified Staged.Stream.Fallible  as S

import Unicode.PrimArray.Sink

#if MIN_VERSION_text(2,0,0)
import Unicode.UTF8.Encoder
#else
import Unicode.UTF16.Encoder
#endif

sinkText
    :: C (IO T.Text)           -- ^ on error
    -> C Int                   -- ^ maximum length to allocate for the underlying array
    -> C a                     -- ^ start element
    -> S.StreamFM IO a Word32  -- ^ stream producing codepoints
    -> C (IO T.Text)
sinkText err len a stream = 
#if MIN_VERSION_text(2,0,0)
   sinkPrimArray err len a (utf8encoder stream) $ \len' p ->
   sreturn [|| case $$p of P.PrimArray ba -> T.Text (T.ByteArray ba) 0 $$len' ||]
#else
   sinkPrimArray err len a (utf16encoder stream) $ \len' p ->
   sreturn [|| case $$p of P.PrimArray ba -> T.Text (T.Array ba) 0 $$len' ||]
#endif
