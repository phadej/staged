{-# LANGUAGE DeriveGeneric #-}
module UTF8.States where

import Data.SOP       (SOP (..))
import Data.Word      (Word32)
import Staged.Commons (C)

import qualified GHC.Generics as GHC

data UTF8DecS xss
    = Start  (SOP C xss)
    | Byte2  (SOP C xss) (C Word32)
    | Byte3a (SOP C xss) (C Word32)
    | Byte3b (SOP C xss) (C Word32)
    | Byte4a (SOP C xss) (C Word32)
    | Byte4b (SOP C xss) (C Word32)
    | Byte4c (SOP C xss) (C Word32)
  deriving (GHC.Generic)
