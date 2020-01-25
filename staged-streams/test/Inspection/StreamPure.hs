{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fplugin=Test.Inspection.Plugin #-}
{-# OPTIONS -dsuppress-all #-}
-- {-# OPTIONS -ddump-splices #-}
module Inspection.StreamPure (tests) where

import Test.Tasty        (TestTree, testGroup)
import Test.Tasty.HUnit  ((@?=), testCase)
import Control.Category  ((>>>))

import Staged.Commons
import qualified Staged.Stream.Pure as S
import qualified Staged.Stream.Pure.Examples as S

import Test.Inspection
import Test.Tasty.Inspection

import Coutts

tests  :: TestTree
tests =  testGroup "Staged.Stream.Pure"
    [ $(inspectCase "toList x id" $ 'ex1lhs === 'ex1rhs)
    , $(inspectCase "toList x (id >>> id)" $ 'ex1lhsB === 'ex1rhs)
    -- note that these produce different amount of states in the generated code,
    -- so optimiser does some work afterwards still.
    , $(inspectCase "foldl $ map . filter . enumFromTo" $ 'ex2lhs  === 'ex2rhs)
    , $(inspectCase "foldl $ mapPipe <<< filter . enumFromTo" $ 'ex2lhsB === 'ex2rhs)
    , $(inspectCase "foldl $ (mapPipe <<< filterPipe) <<< enumFromTo" $ 'ex2lhsC === 'ex2rhs)
    , $(inspectCase "foldl $ mapPipe <<< (filterPipe <<< enumFromTo)" $ 'ex2lhsD === 'ex2rhs)
    , testCase "S.toZero^3 example result" $ do
        ex3lhs 3 @?= [3,2,1,0,2,1,0,1,0,0,2,1,0,1,0,0,1,0,0,0]
    , $(inspectCase "S.toZero^3" $ 'ex3lhs === 'ex3rhs)
    , $(inspectCase "S.toZero^3 reassoc" $ 'ex3lhsB === 'ex3rhs)
    -- zipping
    , $(inspectCase "zipWith ..." $ 'ex4lhs === 'ex4rhs)
    -- Coutts 2007
    , testCase "ex5 10" $ do
        ex5       10 @?= 1705
        ex5coutts 10 @?= 1705
        ex5lhs    10 @?= 1705
        ex5rhs    10 @?= 1705
    , $(inspectCase "coutts ..." $ 'ex5lhs === 'ex5rhs)
    -- , $(inspectCase "coutts model" $ 'ex5lhs === 'ex5) -- doesn't hold.
    -- , $(inspectCase "coutts model" $ 'ex5lhs === 'ex5coutts) -- doesn't hold.
    ]

-------------------------------------------------------------------------------
-- Stated.Stream
-------------------------------------------------------------------------------

-- Basic test
ex1rhs :: a -> [a]
ex1rhs !x = [x]

ex1lhs :: a -> [a]
ex1lhs x = $$(S.toList (C [|| x ||]) S.idPipe)

ex1lhsB :: a -> [a]
ex1lhsB x = $$(S.toList (C [|| x ||]) (S.idPipe >>> S.idPipe))

-- example
ex2rhs :: Int -> Int
ex2rhs hi = go 0 0 where
    go :: Int -> Int -> Int
    go !acc !curr =
        if curr > hi
        then acc
        else if odd curr
             then go (acc + (negate curr)) (1 + curr)
             else go acc (1 + curr)

-- 1 state
ex2lhs :: Int -> Int
ex2lhs n = $$(
      S.foldl (toFn2 [|| (+) ||]) [|| 0 ||] sunit
    $ S.map (toFn [|| negate ||])
    $ S.filter (toFn [|| odd ||])
    $ S.enumFromTo [|| 0 ||] [|| n ||])

-- 2 states
ex2lhsB :: Int -> Int
ex2lhsB n = $$(
    S.foldl (toFn2 [|| (+) ||]) [|| 0 ||] sunit $
    (S.filter (toFn [|| odd ||]) $ S.enumFromTo [|| 0 ||] [|| n ||]) >>>
    S.mapPipe (toFn [|| negate ||]))

-- 6 states
ex2lhsC :: Int -> Int
ex2lhsC n = $$(
    S.foldl (toFn2 [|| (+) ||]) [|| 0 ||] sunit $
    S.enumFromTo [|| 0 ||] [|| n ||] >>>
    S.filterPipe (toFn [|| odd ||]) >>>
    S.mapPipe (toFn [|| negate ||]))

-- 8 states
ex2lhsD :: Int -> Int
ex2lhsD n = $$(
    S.foldl (toFn2 [|| (+) ||]) [|| 0 ||] sunit $
    (S.enumFromTo [|| 0 ||] [|| n ||] >>>
    S.filterPipe (toFn [|| odd ||])) >>>
    S.mapPipe (toFn [|| negate ||]))

-- Composition is quite powerful
ex3lhs :: Int -> [Int]
ex3lhs n = $$(S.toList (C [|| n ||]) $
    S.toZero >>> S.toZero >>> S.toZero) -- 3 states

ex3lhsB :: Int -> [Int]
ex3lhsB n = $$(S.toList (C [|| n ||]) $
    (S.toZero >>> S.toZero) >>> S.toZero) -- 4 states, where 2 same

ex3rhs :: Int -> [Int]
ex3rhs n0 = state0 n0 where
    state0 :: Int -> [Int]
    state0 !n
        | n >= 0    = state1 (n - 1) n
        | otherwise = []

    state1 :: Int -> Int -> [Int]
    state1 !n !m
        | m >= 0    = state2 n (m - 1) m
        | otherwise = state0 n

    state2 :: Int -> Int -> Int -> [Int]
    state2 !n !m !p
        | p >= 0    = p : state2 n m (p - 1)
        | otherwise = state1 n m

-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

ex4lhs :: Int -> Int
ex4lhs n = $$(S.foldl (toFn2  [|| (+) ||]) [|| 0 ||] sunit $
    S.zipWith (toFn2 [|| (*) ||])
    (S.enumFromTo [|| n ||] (liftTyped 5))
    (S.enumFromTo (liftTyped 4) (liftTyped 10)))

ex4rhs :: Int -> Int
ex4rhs n0 = state0 0 n0 4 where
    state0 !acc !x !y =
        if (x > 5)
        then acc
        else state1 acc x (1 + x) y
    state1 !acc !n !x !y =
        if (y > 10)
        then acc
        else state0 (acc + (n * y)) x (1 + y)

-------------------------------------------------------------------------------
-- Coutts 2007 intro example
-------------------------------------------------------------------------------

ex5 :: Int -> Int
ex5 n = sum [ k * m | k <- [1..n], m <- [1..k] ]

ex5lhs :: Int -> Int
ex5lhs n = $$(
     S.foldl splus (liftTyped 0) (C [|| n ||]) $
    (S.enumFromTo' (liftTyped 1)) >>>
    (S.mapWithInput smult (S.enumFromTo' (liftTyped 1))))

ex5rhs :: Int -> Int
ex5rhs = state0 0 1 where
    state0 !acc !k !n =
        if k > n
        then acc
        else state1 acc (1 + k) n k 1 k

    state1 !acc !k n !k2 !m !k3 =
        if m > k3
        then state0 acc k n
        else state1 (acc + k2 * m) k n k2 (1 + m) k3

ex5coutts :: Int -> Int
ex5coutts n = cSum $
    cConcatMap
        (\k -> cConcatMap (\m -> cReturn (k * m)) (cEnumFromTo 1 k))
        (cEnumFromTo 1 n)
