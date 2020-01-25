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
module Data.SOP.Fn.ConcatMapAppend (
    -- * Type families
    ConcatMapAppend,
    -- * Isomorphisms
    concatMapAppend_SOP,
    unconcatMapAppend_SOP,
    concatMapAppend_NSNP,
    unconcatMapAppend_NSNP,
    
    -- * Proxies
    prConcatMapAppend,
    -- * Constraints
    concatMapAppend_SListI2,
    ) where

import Data.Bifunctor (first)

import Data.SOP.Fn.Cons
import Data.SOP.Fn.Append
import Data.SOP.Fn.MapAppend
import Data.SOP.Sh
import Generics.SOP

-------------------------------------------------------------------------------
-- Type families
-------------------------------------------------------------------------------

-- |
--
-- Defined in "Data.SOP.Fn.ConcatMapAppend" module
type family ConcatMapAppend (xss :: [[k]]) (yss :: [[k]]) :: [[k]] where
    ConcatMapAppend '[]         yss = '[]
    ConcatMapAppend (xs ': xss) yss = Append
        (MapAppend xs yss)
        (ConcatMapAppend xss yss)

-------------------------------------------------------------------------------
-- Injections
-------------------------------------------------------------------------------

concatMapAppend_SOP
    :: forall xss yss f. (SListI2 xss, SListI2 yss)
    => SOP f xss
    -> SOP f yss
    -> SOP f (ConcatMapAppend xss yss)
concatMapAppend_SOP (SOP xss) (SOP yss) = SOP (concatMapAppend_NSNP xss yss)

unconcatMapAppend_SOP
    :: forall xss yss f. (SListI2 xss, SListI2 yss)
    => SOP f (ConcatMapAppend xss yss)
    -> (SOP f xss, SOP f yss)
unconcatMapAppend_SOP (SOP p) =
    let (xss, yss) = unconcatMapAppend_NSNP p
    in (SOP xss, SOP yss)

concatMapAppend_NSNP
    :: forall xss yss f. (SListI2 xss, SListI2 yss)
    => NS (NP f) xss
    -> NS (NP f) yss
    -> NS (NP f) (ConcatMapAppend xss yss)
concatMapAppend_NSNP xss@(Z xs) yss
    = mapAppend_SListI2 (prHead xss) yss
    $ injLeft (prConcatMapAppend (prTail xss) yss)
    $ mapAppend_NSNP xs yss

concatMapAppend_NSNP xss@(S xss') yss
    = mapAppend_SListI2 (prHead xss) yss
    $ injRight (prMapAppend (prHead xss) yss)
    $ concatMapAppend_NSNP xss' yss

unconcatMapAppend_NSNP
    :: forall xss yss f. (SListI2 xss, SListI2 yss)
    => NS (NP f) (ConcatMapAppend xss yss)
    -> (NS (NP f) xss, NS (NP f) yss)
unconcatMapAppend_NSNP = go (sh (Proxy :: Proxy xss)) where
    go :: Sh' SListI xss' -> NS (NP f) (ConcatMapAppend xss' yss) -> (NS (NP f) xss', NS (NP f) yss)
    go ShNil            ns = case ns of {}
    go (ShCons xs xss') ns = mapAppend_SListI2 xs yss $ case split_NS' (prMapAppend xs yss) (prConcatMapAppend xss' yss) ns of
        Left  ns' -> first Z (unmapAppend_NSNP ns')
        Right ns' -> first S (go (sh xss') ns')

    yss = Proxy :: Proxy yss

-------------------------------------------------------------------------------
-- Proxies
-------------------------------------------------------------------------------

prConcatMapAppend :: proxy xss -> proxy' yss -> Proxy (ConcatMapAppend xss yss)
prConcatMapAppend _ _  = Proxy

-------------------------------------------------------------------------------
-- constraints
-------------------------------------------------------------------------------

concatMapAppend_SListI2 :: (SListI2 xss, SListI2 yss)
    => proxy xss -> proxy' yss
    -> (SListI2 (ConcatMapAppend xss yss) => r)
    -> r
concatMapAppend_SListI2 xss0 yss0 = go yss0 (sh xss0) where
    go :: (SListI2 xss, SListI2 yss)
       => proxy yss -> Sh xss -> (SListI2 (ConcatMapAppend xss yss) => r) -> r
    go _    ShNil           k = k
    go yss (ShCons xs xss) k
        = mapAppend_SListI2 xs yss
        $ go yss (sh xss)
        $ append_SListI2
            (prMapAppend xs yss)
            (prConcatMapAppend xss yss)
            k
