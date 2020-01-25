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
module Data.SOP.Fn.MapConcat (
    -- * Type families
    MapConcat,
    -- * Isomorphisms
    mapConcat_NSNP,
    unmapConcat_NSNP,
    -- * Proxies
    prMapConcat,
    -- * Constraints
    allMapConcat,
    ) where

import Data.SOP.Fn.All
import Data.SOP.Fn.Concat
import Data.SOP.Sh
import Generics.SOP

-------------------------------------------------------------------------------
-- Type families
-------------------------------------------------------------------------------

-- |
--
-- Defined in "Data.SOP.Fn.MapConcat" module
type family MapConcat (xsss :: [[[k]]]) :: [[k]] where
    MapConcat '[]           = '[]
    MapConcat (xss ': xsss) = Concat xss ': MapConcat xsss

-------------------------------------------------------------------------------
-- NP in NS
-------------------------------------------------------------------------------

mapConcat_NSNP :: NS (NP (NP f)) xsss -> NS (NP f) (MapConcat xsss)
mapConcat_NSNP (Z x) = Z (concat_NP x)
mapConcat_NSNP (S x) = S (mapConcat_NSNP x)

unmapConcat_NSNP :: forall xsss f. SListI3 xsss => NS (NP f) (MapConcat xsss) -> NS (NP (NP f)) xsss
unmapConcat_NSNP zs = case sList :: SList xsss of
    SNil -> case zs of {}
    SCons -> case zs of
        Z x -> Z (unconcat_NP x)
        S x -> S (unmapConcat_NSNP x)

-- TODO: NP in NP
-- TODO: NS in NP
-- TODO: NS in NS

-------------------------------------------------------------------------------
-- Proxies
-------------------------------------------------------------------------------

prMapConcat :: proxy xsss -> Proxy (MapConcat xsss)
prMapConcat _  = Proxy

-------------------------------------------------------------------------------
-- constraints
-------------------------------------------------------------------------------

allMapConcat
    :: (All2 (All c) xsss)
    => proxyC c -> proxy xsss
    -> (All2 c (MapConcat xsss) => r)
    -> r
allMapConcat c0 xsss0 = go c0 (sh xsss0) where
    go :: (All2 (All c) xsss) => proxyC c -> Sh xsss -> (All2 c (MapConcat xsss) => r) -> r
    go _ ShNil           k = k
    go c (ShCons ys yss) k = allConcat c ys $ go c (sh yss) k
