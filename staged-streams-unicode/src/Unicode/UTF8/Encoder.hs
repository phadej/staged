{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE TemplateHaskell #-}
module Unicode.UTF8.Encoder (
    utf8encoder,
) where

import Data.Bits                   (shiftR, (.&.), (.|.))
import Data.SOP                    (SOP)
import Data.Word                   (Word8, Word32)
import Staged.Stream.Fallible.Step (FallibleStep (..))

import Staged.Commons

import qualified GHC.Generics           as GHC
import qualified Staged.Stream.Fallible as S

w32w8 :: C Word32 -> C Word8
w32w8 x = [|| fromIntegral $$x :: Word8 ||]

utf8bytes
    :: C Word32
    -> (C Word8 -> C r)
    -> (C Word8 -> C Word8 -> C r)
    -> (C Word8 -> C Word8 -> C Word8 -> C r)
    -> (C Word8 -> C Word8 -> C Word8 -> C Word8 -> C r)
    -> C r
utf8bytes w32 k1 k2 k3 k4 = 
    [|| if | $$w32 <= 0x7f   -> $$(k1 (w32w8 w32))
           | $$w32 <= 0x7ff  -> $$(k2 (w32w8 (sbor 0xc0 (sshiftR 6  w32))) (w32w8 (top (low6 w32))))
           | $$w32 <= 0xffff -> $$(k3 (w32w8 (sbor 0xe0 (sshiftR 12 w32))) (w32w8 (top (low6 (sshiftR 6  w32)))) (w32w8 (top (low6 w32))))
           | otherwise       -> $$(k4 (w32w8 (sbor 0xf0 (sshiftR 18 w32))) (w32w8 (top (low6 (sshiftR 12 w32)))) (w32w8 (top (low6 (sshiftR 6 w32)))) (w32w8 (top (low6 w32))))
     ||]
  where
    low6 :: C Word32 -> C Word32
    low6 w = [|| $$w .&. 0x3f ||]

    sshiftR :: Int -> C Word32 -> C Word32
    sshiftR n w = [|| shiftR $$w $$(liftTyped n) ||]

    top :: C Word32 -> C Word32
    top = sbor 0x80

    sbor :: Word32 -> C Word32 -> C Word32
    sbor x w = [|| $$w .|. $$(liftTyped x) ||]

utf8encoder :: S.StreamFM m a Word32 -> S.StreamFM m a Word8
utf8encoder (S.MkStreamFM s0 steps0) = S.mkStreamFM (Start . s0) (go steps0) where
    go :: (SOP C xss   -> (FallibleStep (C Word32) (SOP C xss)   -> C (m r)) -> C (m r))
       -> (UTF8Enc xss -> (FallibleStep (C Word8)  (UTF8Enc xss) -> C (m r)) -> C (m r))
    go steps (Start s) k = steps s $ \case
        Fail        -> k Fail
        Stop        -> k Stop
        Skip s'     -> k (Skip (Start s'))
        Emit w32 s' -> utf8bytes w32
            (\b1          -> k $ Emit b1 (Start          s'))
            (\b1 b2       -> k $ Emit b1 (Left1 b2       s'))
            (\b1 b2 b3    -> k $ Emit b1 (Left2 b2 b3    s'))
            (\b1 b2 b3 b4 -> k $ Emit b1 (Left3 b2 b3 b4 s'))

    go _steps (Left1 b1       s) k = k (Emit b1 (Start       s))
    go _steps (Left2 b1 b2    s) k = k (Emit b1 (Left1 b2    s))
    go _steps (Left3 b1 b2 b3 s) k = k (Emit b1 (Left2 b2 b3 s))

data UTF8Enc xss
    = Start (SOP C xss)
    | Left1 (C Word8) (SOP C xss)
    | Left2 (C Word8) (C Word8) (SOP C xss)
    | Left3 (C Word8) (C Word8) (C Word8) (SOP C xss)
  deriving (GHC.Generic)
