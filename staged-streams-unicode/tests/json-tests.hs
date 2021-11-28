{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit ((@?=), testCase)
import Data.ByteString (ByteString)

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Unicode.JSON (unescapeString)

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
    ] 
  where
    example :: ByteString -> String -> TestTree
    example input expected = testCase (T.unpack (TE.decodeUtf8 input)) $ do
        unescapeString input @?= expected
