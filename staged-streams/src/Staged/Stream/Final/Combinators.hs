{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Staged.Stream.Final.Combinators (
    -- * Construction
    singleton,
    fromList,
    unfold,
    iterate,
--    replicate,
    -- * Transformations
    map,
--    mapWithInput,
    lmap,
    filter,
--    take,
--    drop,
    -- * Append
    append,
    empty,
    -- * Zipping
--    zipWith,
    repeat,
    -- * Aligning
--    alignWith,
    -- * Recursion
--    bfsTree,
    -- * Pipes
    idPipe,
--    mapPipe,
--    filterPipe,
    -- * Elimination
    foldl,
    toList,
    ) where

import Prelude (($), Maybe (..), (.))

import Data.Monoid (Ap (..))
import Data.SOP (SOP)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))

import qualified Control.Category as  C

import Symantics

import Staged.Stream.Final.States
import Staged.Stream.Step
import Staged.Stream.Final.Type
import Staged.Stream.Final.Convenience
import Staged.Stream.Final.Internal

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- |
--
singleton :: forall {k} (b :: k) (a :: k) (code :: k -> Type). code b -> StreamG code a b
singleton b = mkStreamG start step where
    start :: code a -> Two code
    start _ = Two1

    step :: Two code -> (Step (code b) (Two code) -> r) -> r
    step Two0 k = k Stop
    step Two1 k = k (Emit b Two0)

-- |
--
-- @
-- 'fromList' :: (C a -> C [b]) -> 'Stream' a b
-- @
fromList :: forall a b code. SymList code => (code a -> code (List_ code b)) -> StreamG code a b
fromList f = unfold f $ \bs k -> caseList_ bs
    (k Nothing)
    (\b bs' -> k (Just (b, bs')))

-- | Unfold
--
-- @
-- 'unfold' :: (C a -> C b) -> (C b -> CPS (Maybe (C c, C b))) -> 'Stream' a c
-- @
unfold
    :: forall a b c code. (SymList code)
    => (code a -> code b)
    -> (forall r. code b -> (Maybe (code c, code b) -> code r) -> code r) -- ^ unfolding
    -> StreamG code a c
unfold start f = MkStreamG (singSOP . start) steps where
    steps :: SOP code '[ '[ b ] ]-> (Step (code c) (SOP code '[ '[ b ] ]) -> code r) -> code r
    steps (unsingSOP -> curr) k = f curr $ \case
        Nothing        -> k Stop
        Just (c, next) -> k (Emit c (singSOP next))

-- |
--
-- @
-- 'iterate' :: (C a -> C a) -> 'Stream' a a
-- @
iterate :: (code a -> code a) -> StreamG code a a
iterate f = MkStreamG singSOP $ \(unsingSOP -> curr) k -> k (Emit curr (singSOP (f curr)))



{-
-- |
--
-- @
-- 'replicate' :: C Int -> C a -> 'Stream' i a
-- @
replicate :: (IsCode Q a ca, IsCode Q Int ci) => ci -> ca -> StreamG i a
replicate i a = take i (repeat a)
-}

-------------------------------------------------------------------------------
-- Transformations
-------------------------------------------------------------------------------

-- |
--
map :: forall a b c term. (term b -> term c) -> StreamG term a b -> StreamG term a c
map f (MkStreamG s0 steps0) = MkStreamG s0 (go steps0) where
    go :: (SOP term xss -> (Step (term b) (SOP term xss) -> term r) -> term r)
       -> (SOP term xss -> (Step (term c) (SOP term xss) -> term r) -> term r)
    go steps s k = steps s $ \case
        Stop      -> k Stop
        Skip   s' -> k (Skip s')
        Emit a s' -> k (Emit (f a) s')

{-
-- |
--
-- @
-- 'mapWithInput' :: (C a -> C b -> C c) -> 'Stream' a b -> 'Stream' a c
-- @
mapWithInput :: forall a b c abc. ToCodeFn2 Q a b c abc => abc -> StreamG a b -> StreamG a c
mapWithInput f (MkStreamG s0 steps0) =
    mkStreamG (\a -> (a, s0 a)) $ \(a, curr) k -> steps0 curr $ \case
        Stop        -> k Stop
        Skip   next -> k (Skip (a, next))
        Emit b next -> k (Emit (toFn2 f a b) (a, next))
-}

-- |
--
-- @
-- 'lmap' :: (C a -> C b) -> 'Stream' b c -> 'Stream' a c
-- @
lmap :: forall a b c code. (code a -> code b) -> StreamG code b c -> StreamG code a c
lmap f (MkStreamG s0 steps0) = MkStreamG (s0 . f) steps0

-- |
--
-- @
-- 'filter' :: (C b -> C Bool) -> 'Stream' a b -> 'Stream' a b
-- @
filter :: forall a b term. SymBool term => (term b -> term (Bool_ term)) -> StreamG term a b -> StreamG term a b
filter p (MkStreamG s0 steps0) = MkStreamG s0 (go steps0) where
    go :: (SOP term xss -> (Step (term b) (SOP term xss) -> term r) -> term r)
       -> (SOP term xss -> (Step (term b) (SOP term xss) -> term r) -> term r)
    go steps s k = steps s $ \case
        Stop      -> k Stop
        Skip   s' -> k (Skip s')
        Emit a s' -> ife_
            (p a)
            (k (Emit a s'))
            (k (Skip s'))

{-
-------------------------------------------------------------------------------
-- Take and drop
-------------------------------------------------------------------------------

-- |
--
-- @
-- 'take' :: C Int -> 'Stream' a b -> 'Stream' a b
-- @
take :: IsCode Q Int n => n -> StreamG a b -> StreamG a b
take n (MkStreamG start steps) =
    mkStreamG (\a -> (toCode [|| 0 ||], start a)) $ \(i, xss) k -> steps xss $ \case
        Stop        -> k Stop
        Skip   xss' -> k (Skip (i, xss'))
        Emit b xss' -> sIfThenElse
            (toCode [|| (<) ||] @@ i @@ toCode n)
            (k (Emit b (toCode [|| (1 +) ||] @@ i, xss')))
            (k Stop)

-- |
--
-- @
-- 'drop' :: C Int -> 'Stream' a b -> 'Stream' a b
-- @
drop :: IsCode Q Int n => n -> StreamG a b -> StreamG a b
drop n (MkStreamG start steps) =
    mkStreamG (\a -> DropL (toCode n) (start a)) $ \step k -> case step of
        DropL m xss -> steps xss $ \case
            Stop        -> k Stop
            Skip   xss' -> k (Skip (DropL m xss'))
            Emit b xss' -> sIfThenElse
                (toCode [|| (0 <) ||] @@ m)
                (k (Skip   (DropL (toCode [|| subtract 1 ||] @@ m) xss')))
                (k (Emit b (DropR xss')))

        DropR xss -> steps xss $ \case
            Stop        -> k Stop
            Skip   xss' -> k (Skip   (DropR xss'))
            Emit b xss' -> k (Emit b (DropR xss'))
-}

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

append :: forall a b term. StreamG term a b -> StreamG term a b -> StreamG term a b
append (MkStreamG startL stepsL) (MkStreamG startR stepsR) =
    mkStreamG (\a -> AppL (Ap a) (startL a)) $ \step k -> case step of
        AppL (Ap a) xss -> stepsL xss $ \case
            Stop        -> k (Skip   (AppR (startR a)))
            Skip   xss' -> k (Skip   (AppL (Ap a) xss'))
            Emit b xss' -> k (Emit b (AppL (Ap a) xss'))

        AppR yss -> stepsR yss $ \case
            Stop -> k Stop
            Skip   yss' -> k (Skip   (AppR yss'))
            Emit b yss' -> k (Emit b (AppR yss'))

empty :: StreamG term a b
empty = mkStreamG (\_ -> One) $ \_ k -> k Stop

{-
-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

-- |
--
-- @
-- 'zipWith' :: (C a -> C b -> C c) -> 'Stream' i a -> 'Stream' i b -> 'Stream' i c
-- @
zipWith
    :: forall i abc a b c. ToCodeFn2 Q a b c abc
    => abc -> StreamG i a -> StreamG i b -> StreamG i c
zipWith h (MkStreamG start0 steps0) (MkStreamG start1 steps1) =
    mkStreamG (\i -> ZipL (start0 i) (start1 i)) (steps steps0 steps1)
  where
    steps
        :: (forall r'. SOP C xss -> (Step (C a) (SOP C xss) -> C r') -> C r')
        -> (forall r'. SOP C yss -> (Step (C b) (SOP C yss) -> C r') -> C r')
        -> Zip a xss yss
        -> (Step (C c) (Zip a xss yss) -> C r)
        -> C r
    steps f _ (ZipL xss yss) k = f xss $ \case
        Stop        -> k Stop
        Skip   xss' -> k (Skip (ZipL xss' yss))
        Emit a xss' -> k (Skip (ZipR a xss' yss))

    steps _ g (ZipR a xss yss) k = g yss $ \case
        Stop        -> k Stop
        Skip   yss' -> k (Skip               (ZipR a xss yss'))
        Emit b yss' -> k (Emit (toFn2 h a b) (ZipL xss yss'))
-}

-- |
--
-- @
-- 'repeat' :: C a -> 'Stream' i a
-- @
repeat :: SymUnit code => code a -> StreamG code i a
repeat a = MkStreamG (\_ -> singSOP unit_) $ \s k -> k (Emit a s)

{-
-------------------------------------------------------------------------------
-- Aligning
-------------------------------------------------------------------------------

-- |
--
-- @
-- 'alignWith' :: (C a -> C c) -> (C b -> C c) -> (C a -> C b -> C c)
--             -> 'Stream' i a -> 'Stream' i b -> 'Stream' i c
-- @
alignWith
    :: forall i a b c ac bc abc. (ToCodeFn Q a c ac, ToCodeFn Q b c bc, ToCodeFn2 Q a b c abc)
    => ac -> bc -> abc
    -> StreamG i a -> StreamG i b -> StreamG i c
alignWith ac bc abc (MkStreamG start0 steps0) (MkStreamG start1 steps1) =
    mkStreamG (\i -> AlignL (start0 i) (start1 i)) (steps steps0 steps1)
  where
    steps
        :: (forall r'. SOP C xss -> (Step (C a) (SOP C xss) -> C r') -> C r')
        -> (forall r'. SOP C yss -> (Step (C b) (SOP C yss) -> C r') -> C r')
        -> Align a xss yss
        -> (Step (C c) (Align a xss yss) -> C r)
        -> C r
    steps f _ (AlignL xss yss) k = f xss $ \case
        Stop        -> k (Skip (AlignDrainR yss))
        Skip   xss' -> k (Skip (AlignL xss' yss))
        Emit a xss' -> k (Skip (AlignR a xss' yss))

    steps _ g (AlignR a xss yss) k = g yss $ \case
        Stop        -> k (Emit (toFn ac a) (AlignDrainL xss))
        Skip   yss' -> k (Skip (AlignR a xss yss'))
        Emit b yss' -> k (Emit (toFn2 abc a b) (AlignL xss yss'))

    steps f _ (AlignDrainL xss) k = f xss $ \case
        Stop        -> k Stop
        Skip   xss' -> k (Skip             (AlignDrainL xss'))
        Emit a xss' -> k (Emit (toFn ac a) (AlignDrainL xss'))

    steps _ g (AlignDrainR yss) k = g yss $ \case
        Stop        -> k Stop
        Skip   yss' -> k (Skip             (AlignDrainR yss'))
        Emit b yss' -> k (Emit (toFn bc b) (AlignDrainR yss'))

-------------------------------------------------------------------------------
-- Recursion
-------------------------------------------------------------------------------

-- |
--
-- @
-- 'bfsTree' :: 'Stream' a a -> (C a -> C Bool) -> 'Stream' a a
-- @
bfsTree
    :: forall a predicate. ToCodeFn Q a Bool predicate
    => StreamG a a    -- ^ endo-stream
    -> predicate     -- ^ whether to recurse on a produced element
    -> StreamG a a
bfsTree (MkStreamG start0 steps0) p = mk start0 steps0 where
    mk  :: forall xss. SListI2 xss
        => (C a -> SOP C xss)
        -> (forall r. SOP C xss -> (Step (C a) (SOP C xss) -> C r) -> C r)
        -> StreamG a a
    mk start1 steps1 = mkStreamG (BfsNext . start1) steps2 where
        steps2  :: BFS a xss -> (Step (C a) (BFS a xss) -> C r) -> C r
        steps2 (BfsNext curr) k = steps1 curr $ \case
            Stop        -> k Stop
            Skip   next -> k (Skip (BfsNext next))
            Emit a next -> sIfThenElse (toFn p a)
                (k (Emit a (BfsStep (scons a snil) next)))
                (k (Emit a (BfsNext next)))

        steps2 (BfsStep as curr) k = steps1 curr $ \case
            Stop -> scaseList as
                (k Stop)
                (\a as' -> k (Skip (BfsStep as' (start1 a))))
            Skip   next -> k (Skip (BfsStep as next))
            Emit a next -> sIfThenElse (toFn p a)
                (k (Emit a (BfsStep (scons a as) next)))
                (k (Emit a (BfsStep as next)))

-------------------------------------------------------------------------------
-- Pipes
-------------------------------------------------------------------------------
-}

-- | Identity.
idPipe :: StreamG code a a
idPipe = C.id

{-
-- | Similar to 'map', prefer using 'map'.
--
-- @
-- 'mapPipe' :: (C a -> C b) -> 'Stream' a b
-- @
mapPipe :: forall a b ab. ToCodeFn Q a b ab => ab -> StreamG a b
mapPipe f = mkStreamG start step where
    start :: C a -> Maybe (C b)
    start a = Just (toFn f a)

    step :: Maybe (C b) -> (Step (C b) (Maybe (C b)) -> C r) -> C r
    step Nothing  k = k Stop
    step (Just b) k = k (Emit b Nothing)

{- "Raw" definition:

MkStreamG (\a -> SOP (Z (C (f a) :* Nil))) step where
    step :: SOP C '[ '[b], '[]] -> (Step (C b) (SOP C ('[ '[b], '[]])) -> C r) -> C r
    step (SOP (Z (C b :* Nil))) k = k (Emit b (SOP (S (Z Nil))))
    step (SOP (S (Z Nil)))      k = k Stop
    step (SOP (S (S ns)))       _ = case ns of {}

-}

-- | Similar to 'filter', prefer using 'filter'.
--
-- @
-- 'filterPipe' :: (C a -> C Bool) -> 'Stream' a a
-- @
filterPipe :: forall a predicate. ToCodeFn Q a Bool predicate => predicate -> StreamG a a
filterPipe p = mkStreamG Just step where
    step :: Maybe (C a) -> (Step (C a) (Maybe (C a)) -> C r) -> C r
    step Nothing  k = k Stop
    step (Just a) k = sIfThenElse
        (toFn p a)
        (k (Emit a Nothing))
        (k Stop) -- stop immediately, don't go to the second state.

{- "Raw" definition:

filterPipe :: forall a. (C a -> C Bool) -> StreamG a a
filterPipe p =  MkStreamG (\a -> SOP (Z (C a :* Nil))) step where
    step :: SOP C '[ '[a], '[]] -> (Step (C a) (SOP C ('[ '[a], '[]])) -> C r) -> C r
    step (SOP (Z (C a :* Nil))) k = sIfThenElse
        (p a)
        (k (Emit a (SOP (S (Z Nil)))))
        (k Stop) -- stop immediately, don't go to the second state!
    step (SOP (S (Z Nil)))      k = k Stop
    step (SOP (S (S ns)))       _ = case ns of {}

-}
-}

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-- |
--
-- @
-- 'foldl' :: (C r -> C b -> C r) -> C r -> C a -> 'Stream' a b -> SpliceQ r
-- @
foldl :: forall r a b code. (SymLetRec code, SymFun code)
    => (code r -> code b -> code r) -> code r -> code a -> StreamG code a b -> code r
foldl op e z (MkStreamG xs steps0) =
    termLetRec1_SOP (body steps0) (xs z) e
  where
    body
        :: (SOP code xss -> (Step (code b) (SOP code xss) -> code r) -> code r)
        -> (SOP code xss -> code r -> code r)
        -> (SOP code xss -> code r -> code r)
    body steps loop curr acc = steps curr $ \case
        Stop        -> acc
        Skip   next -> loop next acc
        Emit b next -> loop next (op acc b)

-- |
--
toList :: forall {k} (a :: k) (b :: k) (expr :: k -> Type). (SymLetRec expr, SymFun expr, SymList expr) => expr a -> StreamG expr a b -> expr (List_ expr b)
toList a (MkStreamG start steps0) =
    termLetRec_SOP (body steps0) (start a)
  where
    body
        :: (SOP expr xss -> (Step (expr b) (SOP expr xss) -> expr (List_ expr b)) -> expr (List_ expr b))
        -> (SOP expr xss -> expr (List_ expr b))
        -> SOP expr xss -> expr (List_ expr b)
    body steps loop curr = steps curr $ \case
        Stop        -> nil_ (Proxy @b)
        Skip   next -> loop next
        Emit b next -> cons_ b (loop next)
