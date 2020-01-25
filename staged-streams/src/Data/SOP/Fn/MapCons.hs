{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.SOP.Fn.MapCons (
    -- * Type families
    MapCons,
    -- * Isomorphisms
    mapCons_NSNP,
    unmapCons_NSNP,
    -- * Proxies
    prMapCons,
    -- * Constraints
    allMapCons,
    ) where

import Data.SOP.Sh
import Generics.SOP

-------------------------------------------------------------------------------
-- Type families
-------------------------------------------------------------------------------

-- |
--
-- Defined in "Data.SOP.Fn.MapCons" module
type family MapCons (x :: k) (xss :: [[k]]) :: [[k]] where
    MapCons x '[]         = '[]
    MapCons x (xs ': xss) = (x ': xs) ': MapCons x xss

-------------------------------------------------------------------------------
-- Isomorphisms
-------------------------------------------------------------------------------

mapCons_NSNP :: f x -> NS (NP f) xss -> NS (NP f) (MapCons x xss)
mapCons_NSNP x (Z xs)  = Z (x :* xs)
mapCons_NSNP x (S xss) = S (mapCons_NSNP x xss)

unmapCons_NSNP :: forall f x xss. SListI2 xss => NS (NP f) (MapCons x xss) -> (f x, NS (NP f) xss)
unmapCons_NSNP xss = case sList :: SList xss of
    SNil  -> case xss of {}
    SCons -> case xss of
        Z (x :* xs) -> (x, Z xs)
        S xss' -> fmap S (unmapCons_NSNP xss') -- fmap maps over snd of a pair

-------------------------------------------------------------------------------
-- Proxies
-------------------------------------------------------------------------------

prMapCons :: proxy x -> proxy' yss -> Proxy (MapCons x yss)
prMapCons _ _  = Proxy

-------------------------------------------------------------------------------
-- constraints
-------------------------------------------------------------------------------

allMapCons
    :: (c x, All2 c xss)
    => proxyC c -> proxy x -> proxy' xss
    -> (All2 c (MapCons x xss) => r)
    -> r
allMapCons c0 xs0 yss0 = go c0 xs0 (shTop yss0) where
    go :: (c x, All2 c yss) => proxyC c -> proxy x -> Sh yss -> (All2 c (MapCons x yss) => r) -> r
    go _ _ ShNil          k = k
    go c p (ShCons _ yss) k = go c p (sh yss) k
