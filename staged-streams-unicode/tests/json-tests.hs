{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad         (forM_)
import Data.ByteString       (ByteString)
import Test.QuickCheck       (counterexample, label, (===))
import Test.Tasty            (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit      (assertEqual, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Text.Printf           (printf)

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8  as BS8
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE

import Unicode.JSON (unescapeText)

import qualified UnescapePure
import qualified UnescapePure2

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

    , testProperty "bytestring2" $ \w8s ->
        let bs = BS.pack w8s -- $ filter (>=0x20) w8s
            lhs = unescapeText_staged bs
            rhs = unescapeText_aeson2 bs
            l   = maybe "failure" (const "success") rhs
        in label l $ lhs === rhs

    , testCase "all-chars" unescapeString
    ]
  where
    example :: ByteString -> T.Text -> TestTree
    example input expected = testCase (T.unpack (TE.decodeUtf8 input)) $ do
        unescapeText_staged input @?= Just expected

unescapeText_staged :: ByteString -> Maybe T.Text
unescapeText_staged = either (const Nothing) Just . unescapeText

unescapeText_aeson :: ByteString -> Maybe T.Text
unescapeText_aeson = either (const Nothing) Just . UnescapePure.unescapeText

unescapeText_aeson2 :: ByteString -> Maybe T.Text
unescapeText_aeson2 = either (const Nothing) Just . UnescapePure2.unescapeText

unescapeString :: IO ()
unescapeString = do
  assertEqual "Basic escaping"
     (Just ("\" / \\ \b \f \n \r \t"))
     (unescapeText_staged "\\\" \\/ \\\\ \\b \\f \\n \\r \\t")

  forM_ [minBound .. maxBound :: Char] $ \ c -> do
    let s = T.pack [c]
    let bs = utf16Char s
    assertEqual (printf "UTF-16 encoded '\\x%X' from '%s'" c (BS8.unpack bs)) (Just s) (unescapeText_staged bs)
  where
    utf16Char :: T.Text -> BS.ByteString
    utf16Char = formatString . Base16.encode . TE.encodeUtf16BE

    formatString :: BS.ByteString -> BS.ByteString
    formatString s
      | BS.length s == 4 = BS.concat ["\\u", s]
      | BS.length s == 8 =
          BS.concat ["\\u", BS.take 4 s, "\\u", BS.drop 4 s, ""]
      | otherwise = error "unescapeString: can't happen"
