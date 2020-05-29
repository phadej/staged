{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- | 'Append' type family, and related stuff for 'NP' and 'NS'.
module Data.SOP.Fn.Append (
    -- * Type family: Append
    Append,
    -- * NP 
    append_NP,
    split_NP,
    split_NP',
    -- * NS
    append_NS,
    split_NS,
    split_NS',
    injLeft,
    injRight,
    -- * Constraints
    append_SListI,
    allAppend,
    append_SListI2,
    -- * Proxies
    prAppend,
    ) where

import Data.SOP
import Data.SOP.Sh

-------------------------------------------------------------------------------
-- Type family
-------------------------------------------------------------------------------

-- Note: This are just terrible, terrible and slow.
-- But they are used only at compile time, so it doesn't matter.

-- |
--
-- Defined in "Data.SOP.Fn.Append" module
type family Append (xs :: [k]) (ys :: [k]) :: [k] where
    Append '[]       ys = ys
    Append (x ': xs) ys = x ': Append xs ys

-------------------------------------------------------------------------------
-- NP
-------------------------------------------------------------------------------

append_NP :: NP f xs -> NP f ys -> NP f (Append xs ys)
append_NP Nil       ys = ys
append_NP (x :* xs) ys = x :* append_NP xs ys

split_NP :: forall xs ys f. SListI xs => NP f (Append xs ys) -> (NP f xs, NP f ys)
split_NP zs = case sList :: SList xs of
    SNil -> (Nil, zs)
    SCons -> case zs of
        x :* zs' -> case split_NP zs' of
            (xs, ys) -> (x :* xs, ys)

split_NP' :: SListI xs => proxy xs -> proxy' ys -> NP f (Append xs ys) -> (NP f xs, NP f ys)
split_NP' _ _ = split_NP

-------------------------------------------------------------------------------
-- NS
-------------------------------------------------------------------------------

append_NS :: forall xs ys f. SListI xs => Either (NS f xs) (NS f ys) -> NS f (Append xs ys)
append_NS (Left xs) = goLeft xs where
    goLeft :: NS f xs' -> NS f (Append xs' ys)
    goLeft (Z x) = Z x
    goLeft (S x) = S (goLeft x)

append_NS (Right ys0) = goRight (Proxy :: Proxy xs) ys0 where
    goRight :: forall xs'. SListI xs' => Proxy xs' -> NS f ys -> NS f (Append xs' ys)
    goRight p ys = case sList :: SList xs' of
        SNil  -> ys
        SCons -> rec p
      where
        rec :: forall x xs''. SListI xs'' => Proxy (x ': xs'') -> NS f (x : Append xs'' ys)
        rec _ = S (goRight (Proxy :: Proxy xs'') ys)

split_NS :: forall xs ys f. SListI xs => NS f (Append xs ys) -> Either (NS f xs) (NS f ys)
split_NS zs = case sList :: SList xs of
    SNil  -> Right zs
    SCons -> case zs of
        Z x -> Left (Z x)
        S z -> case split_NS z of
            Left x  -> Left (S x)
            Right y -> Right y

split_NS'
    :: SListI xs
    => proxy xs -> proxy' ys
    -> NS f (Append xs ys) -> Either (NS f xs) (NS f ys)
split_NS' _ _ = split_NS

injLeft :: forall xs ys f proxy. SListI xs => proxy ys -> NS f xs -> NS f (Append xs ys)
injLeft _ xs = append_NS (Left xs :: Either (NS f xs) (NS f ys))

injRight :: forall xs ys f proxy. SListI xs => proxy xs -> NS f ys -> NS f (Append xs ys)
injRight _ ys = append_NS (Right ys :: Either (NS f xs) (NS f ys))

-------------------------------------------------------------------------------
-- Constraints
-------------------------------------------------------------------------------

append_SListI :: (SListI xs, SListI ys) => proxy xs -> proxy' ys -> (SListI (Append xs ys) => r) -> r
append_SListI = allAppend (Proxy :: Proxy Top)

allAppend :: (All c xs, All c ys) => proxyC c -> proxy xs -> proxy' ys -> (All c (Append xs ys) => r) -> r
allAppend c0 xs ys = go c0 (sh xs) ys where
    go :: (All c xs', All c ys') => proxyC c -> Sh xs' -> proxy' ys' -> (All c (Append xs' ys') => r) -> r
    go _ ShNil          _ k = k
    go c (ShCons _ xs') p k = go c (sh xs') p k

append_SListI2 :: (SListI2 xss, SListI2 yss) => proxy xss -> proxy' yss -> (SListI2 (Append xss yss) => r) -> r
append_SListI2 xss yss = go (sh xss) yss where
    go :: (SListI2 xss', SListI2 yss') => Sh xss' -> proxy' yss' -> (SListI2 (Append xss' yss') => r) -> r
    go ShNil           _ k = k
    go (ShCons _ xss') p k = go (sh xss') p k

-------------------------------------------------------------------------------
-- Proxies
-------------------------------------------------------------------------------

prAppend :: proxy xss -> proxy' yss -> Proxy (Append xss yss)
prAppend _ _ = Proxy
