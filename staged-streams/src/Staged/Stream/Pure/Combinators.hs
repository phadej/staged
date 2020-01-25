{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Staged.Stream.Pure.Combinators (
    -- * Construction
    singleton,
    fromList,
    unfold,
    iterate,
    replicate,
    -- * Transformations
    map,
    mapWithInput,
    lmap,
    filter,
    take,
    drop,
    -- * Append
    append,
    empty,
    -- * Zipping
    zipWith,
    repeat,
    -- * Recursion
    bfsTree,
    -- * Pipes
    idPipe,
    mapPipe,
    filterPipe,
    -- * Elimination
    foldl,
    toList,
    ) where

import Prelude (($), Bool (..), (<), (+), subtract, Int, (.), Maybe (..))
-- import Prelude (undefined)

import Generics.SOP (SOP (..), SListI2)

import qualified Control.Category as  C

import Staged.Commons
import Staged.Stream.States
import Staged.Stream.Step
import Staged.Stream.Pure.Type
import Staged.Stream.Pure.Convenience

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | 
-- 
-- @
-- 'singleton' :: 'C' b -> 'Stream' a b
-- 'singleton' b = 'map' ([|| const ||] @@ b)
-- @
--
singleton :: forall b a value. IsCode b value => value -> Stream a b
singleton b = mkStream start step where
    start _ = True
    step :: Bool -> (Step (C b) Bool -> r) -> r
    step False k = k Stop
    step True  k = k (Emit (toCode b) False)

-- |
--
-- @
-- 'fromList' :: ('C' a -> 'C' [b]) -> 'Stream' a b
-- @
fromList :: ToCodeFn a [b] fn => fn -> Stream a b
fromList f = unfold f $ \bs k -> scaseList bs
    (k Nothing)
    (\b bs' -> k (Just (b, bs')))

unfold
    :: forall a b c fn. ToCodeFn a b fn
    => fn
    -> (forall r. C b -> (Maybe (C c, C b) -> C r) -> C r) -- ^ unfolding
    -> Stream a c
unfold start f = mkStream (toFn start) steps where
    steps :: C b -> (Step (C c) (C b) -> C r) -> C r
    steps curr k = f curr $ \case
        Nothing        -> k Stop
        Just (c, next) -> k (Emit c next)

-- |
--
-- @
-- 'iterate' :: ('C' a -> 'C' a) -> 'C' a -> 'Stream' i a
-- @
iterate :: (IsCode a ca, ToCodeFn a a endo) => endo -> ca -> Stream i a
iterate f z = mkStream (\_ -> toCode z) $ \curr k -> k (Emit curr (toFn f curr))

-- |
--
-- @
-- 'replicate' :: 'C' Int -> 'C' a -> 'Stream' i a
-- @
replicate :: (IsCode a ca, IsCode Int ci) => ci -> ca -> Stream i a
replicate i a = take i (repeat a)

-------------------------------------------------------------------------------
-- Transformations
-------------------------------------------------------------------------------

map :: forall a b c fn. ToCodeFn a b fn => fn -> Stream c a -> Stream c b
map f (MkStream s0 steps0) = MkStream s0 (go steps0) where
    go :: (SOP C xss -> (Step (C a) (SOP C xss) -> C r) -> C r)
       -> (SOP C xss -> (Step (C b) (SOP C xss) -> C r) -> C r)
    go steps s k = steps s $ \case
        Stop      -> k Stop
        Skip   s' -> k (Skip s')
        Emit a s' -> k (Emit (toFn f a) s')

mapWithInput :: forall a b c. (C a -> C b -> C c) -> Stream a b -> Stream a c
mapWithInput f (MkStream s0 steps0) =
    mkStream (\a -> (a, s0 a)) $ \(a, curr) k -> steps0 curr $ \case
        Stop        -> k Stop
        Skip   next -> k (Skip (a, next))
        Emit b next -> k (Emit (f a b) (a, next))

lmap :: forall a b c. (C a -> C b) -> Stream b c -> Stream a c
lmap f (MkStream s0 steps0) = MkStream (s0 . f) steps0

filter :: forall a c pred. ToCodeFn a Bool pred => pred -> Stream c a -> Stream c a
filter p (MkStream s0 steps0) = MkStream s0 (go steps0) where
    go :: (SOP C xss -> (Step (C a) (SOP C xss) -> C r) -> C r)
       -> (SOP C xss -> (Step (C a) (SOP C xss) -> C r) -> C r)
    go steps s k = steps s $ \case
        Stop      -> k Stop
        Skip   s' -> k (Skip s')
        Emit a s' -> sIfThenElse
            (toFn p a)
            (k (Emit a s'))
            (k (Skip s'))

-------------------------------------------------------------------------------
-- Take and drop
-------------------------------------------------------------------------------

take :: IsCode Int n => n -> Stream a b -> Stream a b
take n (MkStream start steps) =
    mkStream (\a -> (C [|| 0 ||], start a)) $ \(i, xss) k -> steps xss $ \case
        Stop        -> k Stop
        Skip   xss' -> k (Skip (i, xss'))
        Emit b xss' -> sIfThenElse
            (C [|| (<) ||] @@ i @@ toCode n)
            (k (Emit b (C [|| (1 +) ||] @@ i, xss')))
            (k Stop)

drop :: IsCode Int n => n -> Stream a b -> Stream a b
drop n (MkStream start steps) =
    mkStream (\a -> DropL (toCode n) (start a)) $ \step k -> case step of
        DropL m xss -> steps xss $ \case
            Stop        -> k Stop
            Skip   xss' -> k (Skip (DropL m xss'))
            Emit b xss' -> sIfThenElse
                (C [|| (0 <) ||] @@ m)
                (k (Skip   (DropL (C [|| subtract 1 ||] @@ m) xss')))
                (k (Emit b (DropR xss')))

        DropR xss -> steps xss $ \case
            Stop        -> k Stop
            Skip   xss' -> k (Skip   (DropR xss'))
            Emit b xss' -> k (Emit b (DropR xss'))

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

append :: forall a b. Stream a b -> Stream a b -> Stream a b
append (MkStream startL stepsL) (MkStream startR stepsR) =
    mkStream (\a -> AppL a (startL a)) $ \step k -> case step of
        AppL a xss -> stepsL xss $ \case
            Stop        -> k (Skip   (AppR (startR a)))
            Skip   xss' -> k (Skip   (AppL a xss'))
            Emit b xss' -> k (Emit b (AppL a xss'))

        AppR yss -> stepsR yss $ \case
            Stop -> k Stop
            Skip   yss' -> k (Skip   (AppR yss'))
            Emit b yss' -> k (Emit b (AppR yss'))
        
empty :: Stream a b
empty = mkStream (\_ -> ()) $ \() k -> k Stop

-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

zipWith :: forall i a b c. (C a -> C b -> C c) -> Stream i a -> Stream i b -> Stream i c
zipWith h (MkStream start0 steps0) (MkStream start1 steps1) =
    mkStream (\i -> ZipL (start0 i) (start1 i)) (steps steps0 steps1)
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
        Skip   yss' -> k (Skip         (ZipR a xss yss'))
        Emit b yss' -> k (Emit (h a b) (ZipL xss yss'))

-- |
--
-- @
-- 'repeat' :: 'C' a -> 'Stream' i a
-- @
repeat :: IsCode a ca => ca -> Stream i a
repeat a = mkStream (\_ -> ()) $ \() k -> k (Emit (toCode a) ())

-------------------------------------------------------------------------------
-- Recursion
-------------------------------------------------------------------------------

bfsTree
    :: forall a.
       Stream a a               -- ^ endo-stream
    -> (C a -> C Bool)  -- ^ whether to recurse on a produced element
    -> Stream a a
bfsTree (MkStream start0 steps0) p = mk start0 steps0 where
    mk  :: forall xss. SListI2 xss
        => (C a -> SOP C xss)
        -> (forall r. SOP C xss -> (Step (C a) (SOP C xss) -> C r) -> C r)
        -> Stream a a
    mk start1 steps1 = mkStream (BfsNext . start1) steps2 where
        steps2  :: BFS a xss -> (Step (C a) (BFS a xss) -> C r) -> C r
        steps2 (BfsNext curr) k = steps1 curr $ \case
            Stop        -> k Stop
            Skip   next -> k (Skip (BfsNext next))
            Emit a next -> sIfThenElse (p a)
                (k (Emit a (BfsStep (scons a snil) next)))
                (k (Emit a (BfsNext next)))

        steps2 (BfsStep as curr) k = steps1 curr $ \case
            Stop -> scaseList as
                (k Stop)
                (\a as' -> k (Skip (BfsStep as' (start1 a))))
            Skip   next -> k (Skip (BfsStep as next))
            Emit a next -> sIfThenElse (p a)
                (k (Emit a (BfsStep (scons a as) next)))
                (k (Emit a (BfsStep as next)))

-------------------------------------------------------------------------------
-- Pipes
-------------------------------------------------------------------------------

idPipe :: Stream a a
idPipe = C.id

-- | Similar to 'map', prefer using 'map'.
mapPipe :: forall a b. (C a -> C b) -> Stream a b
mapPipe f = mkStream start step where
    start :: C a -> Maybe (C b)
    start a = Just (f a)

    step :: Maybe (C b) -> (Step (C b) (Maybe (C b)) -> C r) -> C r
    step Nothing  k = k Stop
    step (Just b) k = k (Emit b Nothing)

{- "Raw" definition:

MkStream (\a -> SOP (Z (C (f a) :* Nil))) step where
    step :: SOP C '[ '[b], '[]] -> (Step (C b) (SOP C ('[ '[b], '[]])) -> C r) -> C r
    step (SOP (Z (C b :* Nil))) k = k (Emit b (SOP (S (Z Nil))))
    step (SOP (S (Z Nil)))      k = k Stop
    step (SOP (S (S ns)))       _ = case ns of {}

-}

-- | Similar to 'filter', prefer using 'filter'.
filterPipe :: forall a. (C a -> C Bool) -> Stream a a
filterPipe p = mkStream Just step where
    step :: Maybe (C a) -> (Step (C a) (Maybe (C a)) -> C r) -> C r
    step Nothing  k = k Stop
    step (Just a) k = sIfThenElse
        (p a)
        (k (Emit a Nothing))
        (k Stop) -- stop immediately, don't go to the second state.

{- "Raw" definition:

filterPipe :: forall a. (C a -> C Bool) -> Stream a a
filterPipe p =  MkStream (\a -> SOP (Z (C a :* Nil))) step where
    step :: SOP C '[ '[a], '[]] -> (Step (C a) (SOP C ('[ '[a], '[]])) -> C r) -> C r
    step (SOP (Z (C a :* Nil))) k = sIfThenElse
        (p a)
        (k (Emit a (SOP (S (Z Nil)))))
        (k Stop) -- stop immediately, don't go to the second state!
    step (SOP (S (Z Nil)))      k = k Stop
    step (SOP (S (S ns)))       _ = case ns of {}

-}

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

foldl :: forall init start r a b. (IsCode r init, IsCode a start)
      => (C r -> C b -> C r) -> init -> start -> Stream a b -> GHCCode r
foldl op e z (MkStream xs steps0) =
    fromCode $ sletrec1_SOP (body steps0) (xs (toCode z)) (toCode e)
  where
    body
        :: (SOP C xss -> (Step (C b) (SOP C xss) -> C r) -> C r)
        -> (SOP C xss -> C r -> C r)
        -> (SOP C xss -> C r -> C r)
    body steps loop curr acc = steps curr $ \case
        Stop        -> acc
        Skip   next -> loop next acc
        Emit b next -> loop next (op acc b)

toList
    :: forall start a b. (IsCode a start)
    => start -> Stream a b -> GHCCode [b]
toList a (MkStream start steps0) =
    fromCode $ sletrec_SOP (body steps0) (start (toCode a))
  where
    body
        :: (SOP C xss -> (Step (C b) (SOP C xss) -> C [b]) -> C [b])
        -> (SOP C xss -> C [b])
        -> SOP C xss -> C [b]
    body steps loop curr = steps curr $ \case
        Stop        -> snil
        Skip   next -> loop next
        Emit b next -> scons b (loop next)
