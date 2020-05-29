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
module Data.SOP.Fn.Concat (
    -- * Type family: Concat
    Concat,
    -- * NP 
    concat_NP,
    unconcat_NP,
    -- * NS
    concat_NS,
    unconcat_NS,
    -- * Constraints
    concat_SListI,
    allConcat,
    -- * Proxies
    prConcat,
    ) where

import Data.SOP
import Data.SOP.Fn.Append
import Data.SOP.Sh

-------------------------------------------------------------------------------
-- Type family
-------------------------------------------------------------------------------

-- |
--
-- Defined in "Data.SOP.Fn.Concat" module
type family Concat (xss :: [[k]]) :: [k] where
    Concat '[]         = '[]
    Concat (xs ': xss) = Append xs (Concat xss)

-------------------------------------------------------------------------------
-- NP
-------------------------------------------------------------------------------

concat_NP :: NP (NP f) xss -> NP f (Concat xss)
concat_NP Nil         = Nil
concat_NP (xs :* xss) = append_NP xs (concat_NP xss)

unconcat_NP :: forall xss f. SListI2 xss => NP f (Concat xss) -> NP (NP f) xss
unconcat_NP zs = case sList :: SList xss of
    SNil  -> Nil
    SCons -> case split_NP zs of
        (xs, ys) -> xs :* unconcat_NP ys

-------------------------------------------------------------------------------
-- NS
-------------------------------------------------------------------------------

concat_NS :: forall xss f. SListI2 xss => NS (NS f) xss -> NS f (Concat xss)
concat_NS ns = case shAll ns of
    ShNil         -> case ns of {}
    ShCons xs xss -> case ns of
        Z zs  -> injLeft (prConcat xss) zs
        S zss -> injRight xs (concat_NS zss)

unconcat_NS :: forall xss f. SListI2 xss => NS f (Concat xss) -> NS (NS f) xss
unconcat_NS ns = case sList :: SList xss of
    SNil  -> case ns of {}
    SCons -> case split_NS ns of
        Left l  -> Z l
        Right r -> S (unconcat_NS r)

-------------------------------------------------------------------------------
-- Constraints
-------------------------------------------------------------------------------

concat_SListI :: SListI2 xss => proxy xss -> (SListI (Concat xss) => r) -> r
concat_SListI = allConcat (Proxy :: Proxy Top)

allConcat :: All2 c xss => proxyC c -> proxy xss -> (All c (Concat xss) => r) -> r
allConcat c' p = go c' (sh p) where
    go :: All2 c yss => proxyC c -> Sh yss -> (All c (Concat yss) => r) -> r
    go _ ShNil           k = k
    go c (ShCons ys yss) k = go c (sh yss) $ allAppend c ys (prConcat yss) k

-------------------------------------------------------------------------------
-- Proxies
-------------------------------------------------------------------------------

prConcat :: proxy xss -> Proxy (Concat xss)
prConcat _ = Proxy
