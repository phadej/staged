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
module Data.SOP.Fn.MapAppend (
    -- * Type families
    MapAppend,
    -- * Isomorphisms
    mapAppend_NSNP,
    unmapAppend_NSNP,
    -- * Proxies
    prMapAppend,
    -- * Constraints
    allMapAppend,
    mapAppend_SListI2,
    ) where

import Data.SOP.Fn.Append
import Data.SOP.Sh
import Generics.SOP

-------------------------------------------------------------------------------
-- Type families
-------------------------------------------------------------------------------

-- |
--
-- Defined in "Data.SOP.Fn.MapAppend" module
type family MapAppend (xs :: [k]) (yss :: [[k]]) :: [[k]] where
    MapAppend xs '[]         = '[]
    MapAppend xs (ys ': yss) = Append xs ys ': MapAppend xs yss

-------------------------------------------------------------------------------
-- Injections
-------------------------------------------------------------------------------

mapAppend_NSNP
    :: NP f xs
    -> NS (NP f) yss
    -> NS (NP f) (MapAppend xs yss)
mapAppend_NSNP xs (Z ys)  = Z (append_NP xs ys)
mapAppend_NSNP xs (S yss) = S (mapAppend_NSNP xs yss)

unmapAppend_NSNP
    :: forall f xs yss. (SListI xs, SListI2 yss)
    => NS (NP f) (MapAppend xs yss)
    -> (NP f xs, NS (NP f) yss)
unmapAppend_NSNP = go (sh (Proxy :: Proxy yss)) where
    go :: Sh yss' -> NS (NP f) (MapAppend xs yss') -> (NP f xs, NS (NP f) yss')
    go ShNil ns = case ns of {}
    go (ShCons _ _)    (Z l) = fmap Z (split_NP l)
    go (ShCons _ yss') (S r) = fmap S (go (sh yss') r)

-------------------------------------------------------------------------------
-- Proxies
-------------------------------------------------------------------------------

prMapAppend :: proxy xs -> proxy' yss -> Proxy (MapAppend xs yss)
prMapAppend _ _  = Proxy

-------------------------------------------------------------------------------
-- constraints
-------------------------------------------------------------------------------

allMapAppend
    :: (All c xs, All2 c xss)
    => proxyC c -> proxy xs -> proxy' xss
    -> (All2 c (MapAppend xs xss) => r)
    -> r
allMapAppend c0 xs0 yss0 = go c0 xs0 (sh yss0) where
    go :: (All c xs, All2 c yss) => proxyC c -> proxy xs -> Sh yss -> (All2 c (MapAppend xs yss) => r) -> r
    go _ _ ShNil           k = k
    go c p (ShCons ys yss) k = allAppend c p ys $ go c p (sh yss) k

mapAppend_SListI2
    :: (SListI xs, SListI2 xss)
    => proxy xs -> proxy' xss
    -> (SListI2 (MapAppend xs xss) => r)
    -> r
mapAppend_SListI2 xs0 yss0 = go xs0 (sh yss0) where
    go :: (SListI xs, SListI2 yss) => proxy xs -> Sh yss -> (SListI2 (MapAppend xs yss) => r) -> r
    go _ ShNil           k = k
    go p (ShCons ys yss) k = append_SListI p ys $ go p (sh yss) k
