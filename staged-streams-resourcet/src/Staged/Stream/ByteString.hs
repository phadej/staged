{-# LANGUAGE TemplateHaskell #-}
module Staged.Stream.ByteString (
    readByteString,
    ) where

import Control.Monad.IO.Class (MonadIO (..))

import qualified Data.ByteString              as BS
import qualified Control.Monad.Trans.Resource as R

import Staged.Commons
import Staged.Stream.Type
import Staged.Stream.Combinators (unfold)
import Staged.Stream.Handle

-- | Open a file and read strict 'ByteString' chunks from it.
readByteString :: R.MonadResource m => StreamM m FilePath BS.ByteString
readByteString
    = withBinaryFile sReadMode
    $ unfold id
    $ \hdl k -> toCode [|| do
        bs <- liftIO (BS.hGetSome $$(fromCode hdl) 32768)
        if BS.null bs
        then $$(fromCode $ k Nothing)
        else $$(fromCode $ k (Just (toCode [|| bs ||], hdl)))
        ||]

-- writeByteString
-- sinkLazy

-------------------------------------------------------------------------------
-- Streams
-------------------------------------------------------------------------------

-- TODO: stdin
-- TODO: stdout
-- TODO: stderr
