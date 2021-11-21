module Main (main) where

import Test.QuickCheck
       (Property, counterexample, maxSuccess, quickCheck, quickCheckWith,
       stdArgs, (===), label)
import Data.Word (Word8)

import UTF8

import qualified Data.ByteString      as BS
import qualified Data.ByteString.UTF8 as UTF8String

test :: String -> IO ()
test = quickCheck . prop

prop :: String -> Property
prop s = counterexample (show $ BS.unpack bs) $
    UTF8String.toString bs === toString bs
  where
    bs = UTF8String.fromString s

prop2 :: [Word8] -> Property
prop2 w8s = label (if elem '\xfffd' rhs  then "Repl" else "NoRepl") $ rhs === lhs
  where
    bs = BS.pack w8s
    rhs = UTF8String.toString bs
    lhs = toString bs

main :: IO ()
main = do
    test "foobar"
    test "\xff\xff"
    test "\xfff\xfff"
    test "\31837"
    quickCheckWith (stdArgs { maxSuccess = 10000 }) prop
    quickCheckWith (stdArgs { maxSuccess = 10000 }) prop2
