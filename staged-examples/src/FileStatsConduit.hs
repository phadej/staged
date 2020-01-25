{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Exception (SomeException, try)

import qualified Data.ByteString              as BS
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Combinators     as C

main :: IO ()
main = conduit

retry :: IO a -> IO a
retry io = do
    ma <- try io
    case ma of
        Right a -> return a
        Left (_ :: SomeException) -> retry io

conduit :: IO ()
conduit = do
    putStrLn "File stats using 'conduit'"
    ---- vvvvv --- without this retry conduit doesn't leak.
    x <- retry $ C.runConduitRes
        $    C.sourceDirectoryDeep True "."
        C..| C.awaitForever C.sourceFileBS
        C..| C.awaitForever (C.unfold BS.uncons)
        C..| C.foldl (\ !n _ -> n + 1) (0 :: Int)
                      --  ^^^   no bang seems to be important.
    print x
