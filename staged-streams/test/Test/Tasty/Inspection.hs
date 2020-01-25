{-# LANGUAGE TemplateHaskell #-}
module Test.Tasty.Inspection (
    inspectCase,
    (===),
    ) where

import           Language.Haskell.TH
import           Test.Inspection     (Obligation, Result (..), inspectTest,
                                      (===))
import           Test.Tasty          (TestName)
import           Test.Tasty.HUnit    (assertFailure, testCase)

inspectCase :: TestName -> Obligation -> ExpQ
inspectCase name obl = [|
    testCase name $ case $(inspectTest obl) of
        Failure str -> assertFailure str
        Success _   -> return ()
    |]
