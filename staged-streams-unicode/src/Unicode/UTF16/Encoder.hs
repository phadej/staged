{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE TemplateHaskell #-}
module Unicode.UTF16.Encoder (
    utf16encoder,
) where

import Data.Bits                   (shiftR, (.&.))
import Data.SOP                    (SOP)
import Data.Word                   (Word16, Word32)
import Staged.Stream.Fallible.Step (FallibleStep (..))

import Staged.Commons

import qualified GHC.Generics           as GHC
import qualified Staged.Stream.Fallible as S

w32w16 :: C Word32 -> C Word16
w32w16 x = [|| fromIntegral $$x :: Word16 ||]

surrogates :: C Word32 -> (C Word16 -> C Word16 -> C r) -> C r
surrogates w32 k =
    [|| let u = $$w32 - 0x10000
            hi = fromIntegral $ 0xd800 + shiftR u 10 :: Word16
            lo = fromIntegral $ 0xdc00 + (u .&. 0x3ff) :: Word16
        in $$(k [|| hi ||] [|| lo ||])
     ||]

utf16encoder :: S.StreamFM m a Word32 -> S.StreamFM m a Word16
utf16encoder (S.MkStreamFM s0 steps0) = S.mkStreamFM (Start . s0) (go steps0) where
    go :: (SOP C xss    -> (FallibleStep (C Word32) (SOP C xss)    -> C (m r)) -> C (m r))
       -> (UTF16Enc xss -> (FallibleStep (C Word16) (UTF16Enc xss) -> C (m r)) -> C (m r))
    go steps (Start s) k = steps s $ \case
        Fail        -> k Fail
        Stop        -> k Stop
        Skip s'     -> k (Skip (Start s'))
        Emit w32 s' ->
            [|| if | $$w32 < 0x10000 -> $$(k $ Emit (w32w16 w32) (Start s'))
                   | otherwise       -> $$(surrogates w32 $ \hi lo ->
                                            k $ Emit hi (Surrogate lo s'))
             ||]

    go _steps (Surrogate lo s) k = k (Emit lo (Start s))

data UTF16Enc xss
    = Start (SOP C xss)
    | Surrogate (C Word16) (SOP C xss)
  deriving (GHC.Generic)
