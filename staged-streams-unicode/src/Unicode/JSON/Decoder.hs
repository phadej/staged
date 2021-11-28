{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE TemplateHaskell #-}
module Unicode.JSON.Decoder (
    jsonStringDecoder,
) where

import Data.Bits                   (shiftL, (.&.), (.|.))
import Data.SOP                    (SOP)
import Data.Word                   (Word32, Word8)
import Staged.Stream.Fallible.Step (FallibleStep (..))

import Staged.Commons

import qualified GHC.Generics           as GHC
import qualified Staged.Stream.Fallible as S

-------------------------------------------------------------------------------
-- Common
-------------------------------------------------------------------------------

sword32 :: Word32 -> C Word32
sword32 w32 = [|| $$(liftTyped w32) :: Word32 ||]

-------------------------------------------------------------------------------
-- Start
-------------------------------------------------------------------------------

w8w32 :: C Word8 -> C Word32
w8w32 x = [|| fromIntegral $$x :: Word32 ||]

-------------------------------------------------------------------------------
-- Byte2
-------------------------------------------------------------------------------

byte2 :: C Word8 -> C Word32
byte2 x = [|| shiftL (fromIntegral ($$x .&. 0x3f)) 6 :: Word32 ||]

byte2end :: C Word32 -> C Word8 -> C Word32
byte2end y x = [|| $$y .|. fromIntegral ($$x .&. 0x3f) :: Word32 ||]

-------------------------------------------------------------------------------
-- Byte3
-------------------------------------------------------------------------------

byte3a :: C Word8 -> C Word32
byte3a x = [|| shiftL (fromIntegral ($$x .&. 0x0f)) 12 :: Word32 ||]

byte3b :: C Word32 -> C Word8 -> C Word32
byte3b y x = [|| $$y .|. shiftL (fromIntegral ($$x .&. 0x3f)) 6 :: Word32 ||]

byte3end :: C Word32 -> C Word8 -> C Word32
byte3end y x = [|| $$y .|. fromIntegral ($$x .&. 0x3f) :: Word32 ||]

-------------------------------------------------------------------------------
-- Byte4
-------------------------------------------------------------------------------

byte4a :: C Word8 -> C Word32
byte4a x = [|| shiftL (fromIntegral ($$x .&. 0x07)) 18 :: Word32 ||]

byte4b :: C Word32 -> C Word8 -> C Word32
byte4b y x = [|| $$y .|. shiftL (fromIntegral ($$x .&. 0x3f)) 12 :: Word32 ||]

byte4c :: C Word32 -> C Word8 -> C Word32
byte4c y x = [|| $$y .|. shiftL (fromIntegral ($$x .&. 0x3f)) 6 :: Word32 ||]

byte4end :: C Word32 -> C Word8 -> C Word32
byte4end y x = [|| $$y .|. fromIntegral ($$x .&. 0x3f) :: Word32 ||]

-------------------------------------------------------------------------------
-- Stream
-------------------------------------------------------------------------------

jsonStringDecoder :: S.StreamFM m a Word8 -> S.StreamFM m a Word32
jsonStringDecoder (S.MkStreamFM s0 steps0) = S.mkStreamFM (Start . s0) (go steps0) where
    go :: (SOP C xss ->    (FallibleStep (C Word8)  (SOP C xss)    -> C r) -> C r)
       -> (UTF8DecS xss -> (FallibleStep (C Word32) (UTF8DecS xss) -> C r) -> C r)
    go steps (Start s) k = steps s $ \case
        Fail       -> k Fail
        Stop       -> k Stop
        Skip s'    -> k (Skip (Start s'))
        Emit w8 s' ->
            [|| if | $$w8 <  0x20 -> $$(k Fail) -- control characters
                   | $$w8 == 0x5c -> $$(k (Skip (Escape s')))
                   | $$w8 <  0x80 -> $$(k (Emit (w8w32 w8) (Start s')))
                   | $$w8 <  0xc0 -> $$(k Fail)
                   | $$w8 <  0xe0 -> $$(k (Skip (Byte2 s' (byte2 w8))))
                   | $$w8 <  0xf0 -> $$(k (Skip (Byte3a s' (byte3a w8))))
                   | $$w8 <  0xf8 -> $$(k (Skip (Byte4a s' (byte4a w8))))
                   | otherwise    -> $$(k Fail)
             ||]

    go steps (Byte2 s acc) k = steps s $ \case
        Fail       -> k Fail
        Stop       -> k Fail
        Skip s'    -> k (Skip (Byte2 s' acc))
        Emit w8 s' ->
            [|| if | $$w8 .&. 0xc0 == 0x80
                   , let acc' = $$(byte2end acc w8)
                   , acc' >= 0x80          -> $$(k (Emit [|| acc' ||] (Start s')))
                   | otherwise             -> $$(k Fail)
             ||]

    go steps (Byte3a s acc) k = steps s $ \case
        Fail       -> k Fail
        Stop       -> k Fail
        Skip s'    -> k (Skip (Byte3a s' acc))
        Emit w8 s' ->
            [|| if | $$w8 .&. 0xc0 == 0x80 -> $$(k (Skip (Byte3b s' (byte3b acc w8))))
                   | otherwise             -> $$(k Fail)
             ||]

    go steps (Byte3b s acc) k = steps s $ \case
        Fail       -> k Fail
        Stop       -> k Fail
        Skip s'    -> k (Skip (Byte2 s' acc))
        Emit w8 s' ->
            [|| if | $$w8 .&. 0xc0 == 0x80
                   , let acc' = $$(byte3end acc w8)
                   , acc' >= 0x800 && acc' < 0xd800 || acc' >= 0xdfff && acc' < 0xfffe
                                           -> $$(k (Emit [|| acc' ||] (Start s')))
                   | otherwise             -> $$(k Fail)
             ||]

    go steps (Byte4a s acc) k = steps s $ \case
        Fail       -> k Fail
        Stop       -> k Fail
        Skip s'    -> k (Skip (Byte4a s' acc))
        Emit w8 s' ->
            [|| if | $$w8 .&. 0xc0 == 0x80 -> $$(k (Skip (Byte4b s' (byte4b acc w8))))
                   | otherwise             -> $$(k Fail)
             ||]

    go steps (Byte4b s acc) k = steps s $ \case
        Fail       -> k Fail
        Stop       -> k Fail
        Skip s'    -> k (Skip (Byte4b s' acc))
        Emit w8 s' ->
            [|| if | $$w8 .&. 0xc0 == 0x80 -> $$(k (Skip (Byte4c s' (byte4c acc w8))))
                   | otherwise             -> $$(k Fail)
             ||]

    go steps (Byte4c s acc) k = steps s $ \case
        Fail       -> k Fail
        Stop       -> k Fail
        Skip s'    -> k (Skip (Byte2 s' acc))
        Emit w8 s' ->
            [|| if | $$w8 .&. 0xc0 == 0x80
                   , let acc' = $$(byte4end acc w8)
                   , acc' >= 0x10000 && acc' < 0x110000
                                           -> $$(k (Emit [|| acc' ||] (Start s')))
                   | otherwise             -> $$(k Fail)
             ||]

    go steps (Escape s) k = steps s $ \case
        Fail       -> k Fail
        Stop       -> k Fail
        Skip s'    -> k (Skip (Escape s')) 
        Emit w8 s' ->
            -- \" represents the quotation mark character (U+0022).
            -- \\ represents the reverse solidus character (U+005C).
            -- \/ represents the solidus character (U+002F).
            -- \b represents the backspace character (U+0008).
            -- \f represents the form feed character (U+000C).
            -- \n represents the line feed character (U+000A).
            -- \r represents the carriage return character (U+000D).
            -- \t represents the character tabulation character (U+0009).
            --
            -- \u starts the codepoint hexadecimal escape
            [|| case $$w8 of
                    0x22 -> $$(k (Emit (sword32 0x22) (Start s'))) -- "
                    0x5c -> $$(k (Emit (sword32 0x5c) (Start s'))) -- \
                    0x2f -> $$(k (Emit (sword32 0x2f) (Start s'))) -- /
                    0x62 -> $$(k (Emit (sword32 0x08) (Start s'))) -- b
                    0x66 -> $$(k (Emit (sword32 0x0c) (Start s'))) -- f
                    0x6e -> $$(k (Emit (sword32 0x0a) (Start s'))) -- n
                    0x72 -> $$(k (Emit (sword32 0x0d) (Start s'))) -- r
                    0x74 -> $$(k (Emit (sword32 0x09) (Start s'))) -- t
                    -- 0x75 -> _ -- u
                    _    -> $$(k Fail)
             ||]

-------------------------------------------------------------------------------
-- State types
-------------------------------------------------------------------------------

data UTF8DecS xss
    = Start  (SOP C xss)
    | Byte2  (SOP C xss) (C Word32)
    | Byte3a (SOP C xss) (C Word32)
    | Byte3b (SOP C xss) (C Word32)
    | Byte4a (SOP C xss) (C Word32)
    | Byte4b (SOP C xss) (C Word32)
    | Byte4c (SOP C xss) (C Word32)
    | Escape (SOP C xss)                            -- ^ after @\\@
--    | EscapeU0 (SOP C xss)                          -- ^ after @\\u@
--    | EscapeU1 (SOP C xss) (C Word32)               -- ^ after @\\ux@
--    | EscapeU2 (SOP C xss) (C Word32)               -- ^ after @\\uxx@
--    | EscapeU3 (SOP C xss) (C Word32)               -- ^ after @\\uxxx@
--    | EscapeSS (SOP C xss) (C Word32)               -- ^ after @\\uxxxx@ where @xxxx@ is a high surrogate
--    | EscapeSU (SOP C xss) (C Word32)               -- ^ after @\\uxxxx\\@ ...
--    | EscapeS0 (SOP C xss) (C Word32)               -- ^ after @\\uxxxx\\u@ ...
--    | EscapeS1 (SOP C xss) (C Word32) (C Word32)    -- ^ after @\\uxxxx\\uy@ ...
--    | EscapeS2 (SOP C xss) (C Word32) (C Word32)    -- ^ after @\\uxxxx\\uyy@ ...
--    | EscapeS3 (SOP C xss) (C Word32) (C Word32)    -- ^ after @\\uxxxx\\uyyy@ ...
  deriving (GHC.Generic)
