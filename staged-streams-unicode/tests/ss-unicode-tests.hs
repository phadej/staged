{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Data.Word             (Word8)
import Test.QuickCheck       (counterexample, label, (===))
import Test.Tasty            (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import qualified Data.ByteString        as BS
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Staged.Stream.Fallible as S

import Unicode.UTF8.Decoder
import Unicode.UTF8.Encoder
import Unicode.Types

recodeUtf8 :: [Word8] -> [Word8]
recodeUtf8 input = unI' $ $$(
    S.toList [|| invalidUtf8 ||] [|| input ||]
    $ utf8encoder
    $ utf8decoder
    $ S.fromList [|| id ||]
    )
  where
    invalidUtf8 :: I' [Word8]
    invalidUtf8 = error "Invalid UTF8"

main :: IO ()
main = defaultMain $ testGroup "ss-unicode-tests"
    [ testProperty "utf8-recode" $ \t ->
        let bytes = BS.unpack $ TE.encodeUtf8 $ T.pack t
        in counterexample (show bytes) $ bytes === recodeUtf8 bytes
    ]
