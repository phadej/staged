module Main (main) where

import Test.QuickCheck
       (Property, counterexample, maxSuccess, quickCheck, quickCheckWith,
       stdArgs, (===), label)
import Data.Word (Word8)

import Unicode

import qualified Data.ByteString      as BS
import qualified Data.ByteString.UTF8 as UTF8

test :: String -> IO ()
test = quickCheck . prop

prop :: String -> Property
prop s = counterexample (show $ BS.unpack bs) $
    UTF8.toString bs === fromUTF8BS bs
  where
    bs = UTF8.fromString s

_prop2 :: [Word8] -> Property
_prop2 w8s = label (if elem '\xfffd' rhs  then "Repl" else "NoRepl") $ rhs === lhs
  where
    bs = BS.pack w8s
    rhs = UTF8.toString bs
    lhs = fromUTF8BS bs

main :: IO ()
main = do
    test "foobar"
    test "\xff\xff"
    test "\xfff\xfff"
    test "\31837"
    quickCheckWith (stdArgs { maxSuccess = 10000 }) prop
    -- this will fail:
    -- quickCheckWith (stdArgs { maxSuccess = 10000 }) _prop2
