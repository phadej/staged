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
module Data.SOP.Fn.Flatten (
    -- * Type family
    FLATTEN,
    -- * Value function
    flatten,
    -- * Isomorpisms
    flatten_NSNP,
    unflatten_NSNP,
    -- * Proxies
    prFLATTEN,
    -- * Constraints
    allFLATTEN,
    ) where

import Data.SOP
import Data.SOP.Fn.All
import Data.SOP.Fn.Append
import Data.SOP.Fn.MapConcat
import Data.SOP.Fn.Sequence
import Data.SOP.Sh

-------------------------------------------------------------------------------
-- value-level function, simple ins't it?
-------------------------------------------------------------------------------

flatten :: [[[[k]]]] -> [[k]]
flatten = concatMap (map concat . sequence)

-------------------------------------------------------------------------------
-- Type-family
-------------------------------------------------------------------------------

-- |
--
-- Defined in "Data.SOP.Fn.Flatten" module
type family FLATTEN (xssss :: [[[[k]]]]) :: [[k]] where
    FLATTEN '[]             = '[]
    FLATTEN (xsss ': xssss) = Append (MapConcat (Sequence xsss)) (FLATTEN xssss)

-------------------------------------------------------------------------------
-- Isomorphisms
-------------------------------------------------------------------------------

flatten_NSNP :: forall xssss f. SListI4 xssss => NS (NP (NS (NP f))) xssss -> NS (NP f) (FLATTEN xssss)
flatten_NSNP ns = case shTop ns of
    ShNil -> case ns of {}
    ShCons prXsss prXssss -> case ns of
        Z xsss -> id
            $ allSequence (prAll prTop) prXsss
            $ allMapConcat prTop (prSequence prXsss)
            $ slistI3to2 xsss
            $ injLeft (prFLATTEN prXssss)
            $ mapConcat_NSNP
            $ sequence_NSNP xsss
        S xssss -> id
            $ allSequence (prAll prTop) prXsss
            $ allMapConcat prTop (prSequence prXsss)
            $ injRight (prMapConcat (prSequence prXsss)) (flatten_NSNP xssss)

unflatten_NSNP :: forall xssss f. SListI4 xssss => NS (NP f) (FLATTEN xssss) -> NS (NP (NS (NP f))) xssss
unflatten_NSNP ns = case shTop (Proxy :: Proxy xssss) of
    ShNil -> case ns of {}
    ShCons prXsss prXssss ->
        allSequence (prAll prTop) prXsss $ allMapConcat prTop (prSequence prXsss) $
        case split_NS' (prMapConcat (prSequence prXsss)) (prFLATTEN prXssss) ns of
            Left l -> Z
                $ slistI3to2 prXsss
                $ unsequence_NSNP
                $ unmapConcat_NSNP l
            Right r -> S (unflatten_NSNP r)

-------------------------------------------------------------------------------
-- Proxies
-------------------------------------------------------------------------------

prFLATTEN :: proxy xssss -> Proxy (FLATTEN xssss)
prFLATTEN _ = Proxy

-------------------------------------------------------------------------------
-- Constraints
-------------------------------------------------------------------------------

allFLATTEN
    :: (All4 c xssss)
    => proxyC c -> proxy xssss
    -> (All2 c (FLATTEN xssss) => r)
    -> r
allFLATTEN c0 xssss0 = go c0 (sh xssss0) where
    go :: All4 c xssss => proxyC c -> Sh xssss -> (All2 c (FLATTEN xssss) => r) -> r
    go _ ShNil              k = k
    go c (ShCons xsss xssss) k
        = allSequence (prAll c) xsss
        $ allMapConcat c (prSequence xsss)
        $ go c (sh xssss)
        $ allAppend (prAll c) (prMapConcat (prSequence xsss)) (prFLATTEN xssss) k

