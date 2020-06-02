{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Main (main) where

import Examples

main :: IO ()
main = do
    print $$(ex01 [|| True ||])
