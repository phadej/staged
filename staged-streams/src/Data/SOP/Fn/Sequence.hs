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
module Data.SOP.Fn.Sequence (
    -- * Type families
    Sequence,
    -- * Isomorphisms
    sequence_NSNP,
    unsequence_NSNP,
    -- * Proxies
    prSequence,
    -- * Constraints
    allSequence,
    ) where

import Data.SOP
import Data.SOP.Fn.All
import Data.SOP.Fn.LiftA2Cons
import Data.SOP.Sh

-------------------------------------------------------------------------------
-- Type families
-------------------------------------------------------------------------------

-- |
--
-- Defined in "Data.SOP.Fn.Sequence" module
type family Sequence (xss :: [[k]]) :: [[k]] where
    Sequence '[]         = '[ '[] ]
    Sequence (xs ': xss) = LiftA2Cons xs (Sequence xss)

-------------------------------------------------------------------------------
-- Isomorphisms
-------------------------------------------------------------------------------

sequence_NSNP :: SListI2 xss => NP (NS f) xss -> NS (NP f) (Sequence xss)
sequence_NSNP Nil         = Z Nil
sequence_NSNP (xs :* xss) = allSequence prTop xss $ liftA2Cons_NS xs (sequence_NSNP xss)

unsequence_NSNP :: forall xss f. SListI2 xss => NS (NP f) (Sequence xss) -> NP (NS f) xss
unsequence_NSNP ns = case shTop (Proxy :: Proxy xss) of
    ShNil -> Nil 
    ShCons _ prXs -> allSequence prTop prXs $ case unliftA2Cons_NS ns of
        (x, xs) -> x :* unsequence_NSNP xs

-------------------------------------------------------------------------------
-- Proxies
-------------------------------------------------------------------------------

prSequence :: proxy xss -> Proxy (Sequence xss)
prSequence _  = Proxy

-------------------------------------------------------------------------------
-- Constraints
-------------------------------------------------------------------------------

allSequence
    :: (All2 c xss)
    => proxyC c -> proxy xss
    -> (All2 c (Sequence xss) => r)
    -> r
allSequence c0 xss0 = go c0 (sh xss0) where
    go :: All2 c xss => proxyC c -> Sh xss -> (All2 c (Sequence xss) => r) -> r
    go _ ShNil           k = k
    go c (ShCons xs xss) k = go c (sh xss) $ allLiftA2Cons c xs (prSequence xss) k
