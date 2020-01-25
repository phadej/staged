module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Inspection.StreamPure

main :: IO ()
main = defaultMain $ testGroup "staged inspection"
    [ Inspection.StreamPure.tests
    ]
