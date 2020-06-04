{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Main (main) where

import Examples

import Staged.Commons

main :: IO ()
main = do
    -- simple example
    print $$(ex01 [|| True ||])

    -- letrec and letrecH
    print $$(fromCode $ fibnr 8)
    print $ $$(fromCode $ isEvenCodeH TagE) (10 :: Int)
