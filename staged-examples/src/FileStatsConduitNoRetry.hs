{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import qualified Data.ByteString          as BS
import qualified Data.Conduit             as C
import qualified Data.Conduit.Combinators as C

main :: IO ()
main = conduit

conduit :: IO ()
conduit = do
    putStrLn "File stats using 'conduit'; without retry"
    x <- C.runConduitRes
        $    C.sourceDirectoryDeep True "."
        C..| C.awaitForever C.sourceFileBS
        C..| C.awaitForever (C.unfold BS.uncons)
        C..| C.foldl (\ !n _ -> n + 1) (0 :: Int)
    print x
