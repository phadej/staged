module Main (main) where

import Data.Word       (Word8)
import Test.QuickCheck
       (Property, counterexample, label, maxSuccess, quickCheck, quickCheckWith,
       stdArgs, (===))

import Unicode

import qualified Data.ByteString      as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Text            as T

test :: String -> IO ()
test s = do
    quickCheck $ prop1 s
    quickCheck $ prop3 s

prop1 :: String -> Property
prop1 s = counterexample (show $ BS.unpack bs) $
    UTF8.toString bs === fromUTF8BS bs
  where
    bs = UTF8.fromString s

_prop2 :: [Word8] -> Property
_prop2 w8s = label (if elem '\xfffd' rhs  then "Repl" else "NoRepl") $ rhs === lhs
  where
    bs = BS.pack w8s
    rhs = UTF8.toString bs
    lhs = fromUTF8BS bs

prop3 :: String -> Property
prop3 s = counterexample (show $ BS.unpack bs) $
    T.pack s === textFromUTF8BS bs
  where
    bs = UTF8.fromString s

main :: IO ()
main = do
    test "foobar"
    test "\xff\xff"
    test "\xfff\xfff"
    test "\31837"
    test "\65536"
    quickCheckWith (stdArgs { maxSuccess = 10000 }) prop1
    quickCheckWith (stdArgs { maxSuccess = 10000 }) prop3
    -- this will fail:
    -- quickCheckWith (stdArgs { maxSuccess = 10000 }) _prop2
