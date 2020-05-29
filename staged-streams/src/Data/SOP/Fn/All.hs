{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE GADTs #-}
module Data.SOP.Fn.All where

import Data.SOP
import Data.SOP.Sh

prAll :: proxy c -> Proxy (All c)
prAll _ = Proxy

prTop :: Proxy Top
prTop = Proxy

slistI2to1
    :: SListI2 xss
    => proxy xss
    -> (SListI xss => r)
    -> r
slistI2to1 p = go (sh p) where
    go :: Sh xss -> (SListI xss => r) -> r
    go ShNil          k = k
    go (ShCons _ xss) k = go (sh xss) k

slistI3to2
    :: SListI3 xsss
    => proxy xsss
    -> (SListI2 xsss => r)
    -> r
slistI3to2 p = go (sh p) where
    go :: SListI3 xsss => Sh xsss -> (SListI2 xsss => r) -> r
    go ShNil           k = k
    go (ShCons _ xsss) k = go (sh xsss) k

slistI4to3
    :: SListI4 xssss
    => proxy xssss
    -> (SListI3 xssss => r)
    -> r
slistI4to3 p = go (sh p) where
    go :: SListI4 xssss => Sh xssss -> (SListI3 xssss => r) -> r
    go ShNil               k = k
    go (ShCons xsss xssss) k = slistI3to2 xsss $ go (sh xssss) k

type All3 c = All (All (All c))
type SListI3 = All3 Top

type All4 c = All (All (All (All c)))
type SListI4 = All4 Top
