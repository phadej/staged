{-# LANGUAGE TemplateHaskellQuotes #-}
module Staged.Stream.Handle (
    -- * Opening files
    withBinaryFile,
    -- * Lifted IOMode
    sReadMode,
    sWriteMode,
    sAppendMode,
    sReadWriteMode
    ) where

import System.IO (Handle, IOMode (..), hClose, openBinaryFile)

import qualified Control.Monad.Trans.Resource as R

import Staged.Commons
import Staged.Stream.Type
import Staged.Stream.ResourceT

-------------------------------------------------------------------------------
-- Opening files
-------------------------------------------------------------------------------

withBinaryFile
    :: R.MonadResource m
    => C IOMode
    -> StreamM m Handle c
    -> StreamM m FilePath c
withBinaryFile mode = bracket
    (\f -> toCode [|| openBinaryFile ||] @@ f @@ mode)
    (\hdl -> toCode [|| hClose ||] @@ hdl)

-------------------------------------------------------------------------------
-- IOModes
-------------------------------------------------------------------------------

sReadMode      :: C IOMode
sReadMode      = toCode [|| ReadMode ||]

sWriteMode     :: C IOMode
sWriteMode     = toCode [|| WriteMode ||]

sAppendMode    :: C IOMode
sAppendMode    = toCode [|| AppendMode ||]

sReadWriteMode :: C IOMode
sReadWriteMode = toCode [|| ReadWriteMode ||]
