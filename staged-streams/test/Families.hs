{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.Maybe                (fromMaybe)
import GHC.TypeLits              (KnownNat, SomeNat (..))
import Numeric.Natural
import Test.QuickCheck           (Property, (===))
import Test.QuickCheck.Instances ()
import Test.Tasty                (defaultMain, testGroup)
import Test.Tasty.QuickCheck     (testProperty)

import Data.SOP.Fn.Append
import Data.SOP.Fn.Concat
import Data.SOP.Fn.MapAppend
import Data.SOP.Sh

import Generics.SOP

import qualified GHC.TypeLits

-------------------------------------------------------------------------------
-- Compat
-------------------------------------------------------------------------------

someNatVal :: Natural -> SomeNat
someNatVal n
    = fromMaybe (error $ "someNatVal: " ++ show n)
    $ GHC.TypeLits.someNatVal (fromIntegral n)

natVal :: KnownNat n => proxy n -> Natural
natVal = fromInteger . GHC.TypeLits.natVal

-------------------------------------------------------------------------------
-- Properties
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "Data.SOP.Families"
    [ testProperty "Append" append_prop
    , testProperty "Concat" concat_prop
    , testProperty "MapAppend" mapAppend_prop
    ]

append_prop :: [Natural] -> [Natural] -> Property
append_prop ns ms = lhs === rhs where
    rhs = ns ++ ms
    lhs =
        reifyNatList ns $ \ns' ->
        reifyNatList ms $ \ms' ->
        allAppend (Proxy :: Proxy KnownNat) ns' ms' $
        reflectNatList (prAppend ns' ms')

concat_prop :: [[Natural]] -> Property
concat_prop nss = lhs === rhs where
    rhs = concat nss
    lhs =
        reifyNatListList nss $ \nss' ->
        allConcat (Proxy :: Proxy KnownNat) nss' $
        reflectNatList (prConcat nss')

mapAppend_prop :: [Natural] -> [[Natural]] -> Property
mapAppend_prop ns mss = lhs === rhs where
    rhs = map (ns ++) mss
    lhs =
        reifyNatList ns $ \ns' ->
        reifyNatListList mss $ \mss' ->
        allMapAppend (Proxy :: Proxy KnownNat) ns' mss' $
        reflectNatListList (prMapAppend ns' mss')

-- foo []       _   = []
-- foo (xs:xss) yss = xs : map (xs ++) yss ++ foo xss yss

-------------------------------------------------------------------------------
-- Reify & Reflect
-------------------------------------------------------------------------------

reifyNat :: Natural -> (forall n. KnownNat n => Proxy n -> r) -> r
reifyNat n k = case someNatVal n of
    SomeNat n' -> k n'

-- reflectNat = natVal

reifyList
    :: Proxy c
    -> (x -> (forall y. c y => Proxy y -> r) -> r)
    -> [x] -> (forall ys. All c ys => Sh' c ys -> r) -> r
reifyList _ _     []     k = k ShNil
reifyList c reify (x:xs) k =
    reify x $ \y ->
    reifyList c reify xs $ \ys ->
    k (ShCons y (toProxy ys))

reifyNatList :: [Natural] -> (forall xs. All KnownNat xs => Sh' KnownNat xs -> r) -> r
reifyNatList = reifyList (Proxy :: Proxy KnownNat) reifyNat

reifyNatListList :: [[Natural]] -> (forall xss. All2 KnownNat xss => Sh' (All KnownNat) xss -> r) -> r
reifyNatListList =
    reifyList (Proxy :: Proxy (All KnownNat)) $ \xs k ->
    reifyNatList xs $ \s ->
    k (toProxy s)

reflectList
    :: forall x ys proxy c. All c ys
    => Proxy c
    -> (forall y proxy'. c y => proxy' y -> x)
    -> proxy ys -> [x]
reflectList _ reflect ys = go (sh ys) where
    go :: All c zs => Sh' c zs -> [x]
    go ShNil         = []
    go (ShCons x xs) = reflect x : go (sh xs)

reflectNatList :: All KnownNat xs => proxy xs -> [Natural]
reflectNatList = reflectList (Proxy :: Proxy KnownNat) natVal

reflectNatListList :: All2 KnownNat xss => Proxy xss -> [[Natural]]
reflectNatListList = reflectList (Proxy :: Proxy (All KnownNat)) reflectNatList

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

toProxy :: proxy xs -> Proxy xs
toProxy _ = Proxy
