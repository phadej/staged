{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.ByteString  (ByteString)
import Test.QuickCheck  (counterexample, label, (===))
import Test.Tasty       (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

import qualified Data.ByteString    as BS
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

import Unicode.JSON (unescapeText)

import qualified UnescapePure

main :: IO ()
main = defaultMain $ testGroup "json-tests"
    [ example "abcd"      "abcd"

    -- utf8
    , example "\xc2\xa2"  "¢"

    -- escapes
    , example "\\\""      "\""
    , example "\\\\"      "\\"
    , example "\\/"       "/"
    , example "\\b"       "\b"
    , example "\\f"       "\f"
    , example "\\n"       "\n"
    , example "\\r"       "\r"
    , example "\\t"       "\t"

    -- u-escapes
    , example "\\u0000"   "\0"
    , example "\\u1234"   "\x1234"
    , example "\\uaAaA"   "\xAAAA"
    , example "\\uFFFF"   "\xFFFF"

    , example "\240\157\132\158" "𝄞"
    , example "\\uD834\\uDD1E"   "𝄞"

    , testProperty "bytestring" $ \w8s ->
        let bs = BS.pack w8s -- $ filter (>=0x20) w8s
            lhs = unescapeText_staged bs
            rhs = unescapeText_aeson bs
            l   = maybe "failure" (const "success") rhs
        in label l $ lhs === rhs
    ]
  where
    example :: ByteString -> T.Text -> TestTree
    example input expected = testCase (T.unpack (TE.decodeUtf8 input)) $ do
        unescapeText_staged input @?= Just expected

unescapeText_staged :: ByteString -> Maybe T.Text
unescapeText_staged = either (const Nothing) Just . unescapeText

unescapeText_aeson :: ByteString -> Maybe T.Text
unescapeText_aeson = either (const Nothing) Just . UnescapePure.unescapeText
