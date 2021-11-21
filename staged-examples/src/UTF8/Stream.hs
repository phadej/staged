{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE TemplateHaskell #-}
module UTF8.Stream (
    decoder,
    ) where

import Data.Bits          (shiftL, (.&.), (.|.))
import Data.SOP           (SOP)
import Data.Word          (Word32, Word8)
import Staged.Stream.Step (Step (..))

import Staged.Commons

import qualified Staged.Stream.Pure as S

import UTF8.States

w8w32 :: C Word8 -> C Word32
w8w32 x = [|| fromIntegral $$x :: Word32 ||]

replacementChar :: C Word32
replacementChar = [|| 0xfffd :: Word32 ||]

byte2 :: C Word8 -> C Word32
byte2 x = [|| shiftL (fromIntegral ($$x .&. 0x3f)) 6 ||]

byte2end :: C Word32 -> C Word8 -> C Word32
byte2end y x = [|| $$y .|. fromIntegral ($$x .&. 0x3f) ||]

byte3a :: C Word8 -> C Word32
byte3a x = [|| shiftL (fromIntegral ($$x .&. 0x0f)) 12 ||]

byte3b :: C Word32 -> C Word8 -> C Word32
byte3b y x = [|| $$y .|. shiftL (fromIntegral ($$x .&. 0x3f)) 6 ||]

byte3end :: C Word32 -> C Word8 -> C Word32
byte3end y x = [|| $$y .|. fromIntegral ($$x .&. 0x3f) ||]

byte4a :: C Word8 -> C Word32
byte4a x = [|| shiftL (fromIntegral ($$x .&. 0x07)) 18 ||]

byte4b :: C Word32 -> C Word8 -> C Word32
byte4b y x = [|| $$y .|. shiftL (fromIntegral ($$x .&. 0x3f)) 12 ||]

byte4c :: C Word32 -> C Word8 -> C Word32
byte4c y x = [|| $$y .|. shiftL (fromIntegral ($$x .&. 0x3f)) 6 ||]

byte4end :: C Word32 -> C Word8 -> C Word32
byte4end y x = [|| $$y .|. fromIntegral ($$x .&. 0x3f) ||]

decoder :: S.Stream a Word8 -> S.Stream a Word32
decoder (S.MkStream s0 steps0) = S.mkStream (Start . s0) (go steps0) where
    go :: (SOP C xss ->    (Step (C Word8)  (SOP C xss)    -> C r) -> C r)
       -> (UTF8DecS xss -> (Step (C Word32) (UTF8DecS xss) -> C r) -> C r)
    go steps (Start s) k = steps s $ \case
        Stop       -> k Stop
        Skip s'    -> k (Skip (Start s'))
        Emit w8 s' ->
            [|| if | $$w8 < 0x80 -> $$(k (Emit (w8w32 w8) (Start s')))
                   | $$w8 < 0xc0 -> $$(k (Emit replacementChar (Start s')))
                   | $$w8 < 0xe0 -> $$(k (Skip (Byte2 s' (byte2 w8))))
                   | $$w8 < 0xf0 -> $$(k (Skip (Byte3a s' (byte3a w8))))
                   | $$w8 < 0xf8 -> $$(k (Skip (Byte4a s' (byte4a w8))))
                   | otherwise   -> $$(k (Emit replacementChar (Start s')))
             ||]

    go steps (Byte2 s acc) k = steps s $ \case
        Stop       -> k (Emit replacementChar (Start s))
        Skip s'    -> k (Skip (Byte2 s' acc))
        Emit w8 s' ->
            [|| if | $$w8 .&. 0xc0 == 0x80
                   , let acc' = $$(byte2end acc w8)
                   , acc' >= 0x80          -> $$(k (Emit [|| acc' ||] (Start s')))
                   | otherwise             -> $$(k (Emit replacementChar (Start s')))
             ||]

    go steps (Byte3a s acc) k = steps s $ \case
        Stop       -> k (Emit replacementChar (Start s))
        Skip s'    -> k (Skip (Byte3a s' acc))
        Emit w8 s' ->
            [|| if | $$w8 .&. 0xc0 == 0x80 -> $$(k (Skip (Byte3b s' (byte3b acc w8))))
                   | otherwise             -> $$(k (Emit replacementChar (Start s')))
             ||]

    go steps (Byte3b s acc) k = steps s $ \case
        Stop       -> k (Emit replacementChar (Start s))
        Skip s'    -> k (Skip (Byte2 s' acc))
        Emit w8 s' ->
            [|| if | $$w8 .&. 0xc0 == 0x80
                   , let acc' = $$(byte3end acc w8)
                   , acc' >= 0x800 && acc' < 0xd800 || acc' >= 0xdfff && acc' < 0xfffe
                                           -> $$(k (Emit [|| acc' ||] (Start s')))
                   | otherwise             -> $$(k (Emit replacementChar (Start s')))
             ||]

    go steps (Byte4a s acc) k = steps s $ \case
        Stop       -> k (Emit replacementChar (Start s))
        Skip s'    -> k (Skip (Byte4a s' acc))
        Emit w8 s' ->
            [|| if | $$w8 .&. 0xc0 == 0x80 -> $$(k (Skip (Byte4b s' (byte4b acc w8))))
                   | otherwise             -> $$(k (Emit replacementChar (Start s')))
             ||]

    go steps (Byte4b s acc) k = steps s $ \case
        Stop       -> k (Emit replacementChar (Start s))
        Skip s'    -> k (Skip (Byte4b s' acc))
        Emit w8 s' ->
            [|| if | $$w8 .&. 0xc0 == 0x80 -> $$(k (Skip (Byte4c s' (byte4c acc w8))))
                   | otherwise             -> $$(k (Emit replacementChar (Start s')))
             ||]

    go steps (Byte4c s acc) k = steps s $ \case
        Stop       -> k (Emit replacementChar (Start s))
        Skip s'    -> k (Skip (Byte2 s' acc))
        Emit w8 s' ->
            [|| if | $$w8 .&. 0xc0 == 0x80
                   , let acc' = $$(byte4end acc w8)
                   , acc' >= 0x10000 && acc' < 0x110000
                                           -> $$(k (Emit [|| acc' ||] (Start s')))
                   | otherwise             -> $$(k (Emit replacementChar (Start s')))
             ||]
