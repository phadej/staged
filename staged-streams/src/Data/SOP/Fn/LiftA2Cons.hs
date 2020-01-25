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
module Data.SOP.Fn.LiftA2Cons (
    -- * Type families
    LiftA2Cons,
    -- * Isomorphisms
    liftA2Cons_NS,
    unliftA2Cons_NS,
    -- * Proxies
    prLiftA2Cons,
    -- * Constraints
    allLiftA2Cons,
    ) where

import Data.Bifunctor (first)
import Data.SOP.Fn.All
import Data.SOP.Fn.Append
import Data.SOP.Fn.MapCons
import Data.SOP.Sh
import Generics.SOP

-------------------------------------------------------------------------------
-- Type family
-------------------------------------------------------------------------------

-- |
--
-- Defined in "Data.SOP.Fn.LiftA2Cons" module
type family LiftA2Cons (xs :: [k]) (yss :: [[k]]) :: [[k]] where
    LiftA2Cons '[]       yss = '[]
    LiftA2Cons (x ': xs) yss = Append (MapCons x yss) (LiftA2Cons xs yss)

-------------------------------------------------------------------------------
-- Isomorphisms
-------------------------------------------------------------------------------

liftA2Cons_NS :: forall xs yss f. (SListI xs, SListI2 yss) => NS f xs -> NS (NP f) yss -> NS (NP f) (LiftA2Cons xs yss)
liftA2Cons_NS ns yss = case shTop ns of
    ShNil -> case ns of {}
    ShCons x xs -> case ns of
        Z z  -> allMapCons prTop x prYss $ injLeft (prLiftA2Cons xs prYss) $ mapCons_NSNP z yss
        S zs -> allMapCons prTop x prYss $ injRight (prMapCons x prYss) $ liftA2Cons_NS zs yss
  where
    prYss = Proxy :: Proxy yss

unliftA2Cons_NS :: forall xs yss f. (SListI xs, SListI2 yss) => NS (NP f) (LiftA2Cons xs yss) -> (NS f xs, NS (NP f) yss)
unliftA2Cons_NS ns = case shTop (Proxy :: Proxy xs) of
    ShNil -> case ns of {}
    ShCons prX _ -> allMapCons prTop prX prYss $ case split_NS ns of 
        Left l  -> first Z $ unmapCons_NSNP l
        Right r -> first S $ unliftA2Cons_NS r
  where
    prYss = Proxy :: Proxy yss

-------------------------------------------------------------------------------
-- Proxies
-------------------------------------------------------------------------------

prLiftA2Cons :: proxy xs -> proxy' yss -> Proxy (LiftA2Cons xs yss)
prLiftA2Cons _ _  = Proxy

-------------------------------------------------------------------------------
-- Constraints
-------------------------------------------------------------------------------

allLiftA2Cons
    :: (All c xs, All2 c yss)
    => proxyC c -> proxy xs -> proxy' yss
    -> (All2 c (LiftA2Cons xs yss) => r)
    -> r
allLiftA2Cons c0 xs0 yss0 = go c0 (sh xs0) yss0 where
    go :: (All c xs, All2 c yss) => proxyC c -> Sh xs -> proxy' yss -> (All2 c (LiftA2Cons xs yss) => r) -> r
    go _ ShNil         _   k = k
    go c (ShCons x xs) yss k
        = allMapCons c x yss
        $ go c (sh xs) yss
        $ allAppend (prAll c) (prMapCons x yss) (prLiftA2Cons xs yss) k 
