{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Staged.Stream.Combinators (
    -- * Construction
    singleton,
    fromList,
    fromListM,
    unfold,
    iterate,
    -- * Transformations
    map,
    mapWithInput,
    lmap,
    traverse,
    filter,
    filterM,
    drop,
    -- * Append
    append,
    empty,
    -- * Zipping
    zipWith,
    repeat,
    -- ** Recursion
    bfsTreeM,
    -- * Pipes
    idPipe,
    mapPipe,
    traversePipe,
    filterPipe,
    -- * Elimination
    run,
    foldl,
    foldlM,
    toList,
    -- * Conversion
    fromPure,
    toPure,
    ) where

import Prelude (($), Bool(..), Either (..), Int, subtract, (<), (.), Maybe (..), Monad)
-- import Prelude (undefined)

import Generics.SOP (SOP (..), SListI2)
import Data.Functor.Identity (Identity (..))

import qualified Control.Category as  C

import Staged.Commons
import Staged.Stream.Step
import Staged.Stream.States
import Staged.Stream.Pure.Type
import Staged.Stream.Type
import Staged.Stream.Convenience

import qualified Staged.Stream.Pure as Pure

-------------------------------------------------------------------------------
-- Conversion
-------------------------------------------------------------------------------

-- | Convert from pure 'Stream' to monadic 'StreamM'.
fromPure :: Stream a b -> StreamM m a b
fromPure (MkStream start step) = MkStreamM start step

-- | Convert from @'StreamM' 'Identity'@ to 'Stream'.
--
-- /Note:/ while 'fromPure' conversion is /clean/,
-- 'toPure' will add 'runIdentity' and 'Identity' into the splices.
--
toPure :: StreamM Identity a b -> Stream a b
toPure (MkStreamM start steps) = MkStream start $ \curr k ->
    C [|| runIdentity ||] @@ (steps curr $ \s -> C [|| Identity ||] @@ k s )

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- |
--
-- @
-- 'singleton' b = 'map' ([|| const ||] @@ b)
-- @
singleton :: forall b a m. C b -> StreamM m a b
singleton = fromPure . Pure.singleton

fromList :: (C a -> C [b]) -> StreamM m a b
fromList = fromPure . Pure.fromList

fromListM :: forall a b m. Monad m => (C a -> C (m [b])) -> StreamM m a b
fromListM f = mkStreamM (Left . f) steps where
    steps
        :: Either (C (m [b])) (C [b])
        -> (Step (C b) (Either (C (m [b])) (C [b])) -> C (m r))
        -> C (m r)
    steps (Left mbs) k = mbs >>>= \bs -> k (Skip (Right bs))
    steps (Right mb) k = scaseList mb
        (k Stop)
        (\b bs' -> k (Emit b (Right bs')))

unfold
    :: forall a b c m.
       (C a -> C b) -- ^ initial state
    -> (forall r. C b -> (Maybe (C c, C b) -> C (m r)) -> C (m r)) -- ^ unfolding
    -> StreamM m a c
unfold start f = mkStreamM start steps where
    steps :: C b -> (Step (C c) (C b) -> C (m r)) -> C (m r)
    steps curr k = f curr $ \case
        Nothing        -> k Stop
        Just (c, next) -> k (Emit c next)

iterate :: (C a -> C a) -> C a -> StreamM m i a
iterate f z = fromPure (Pure.iterate f z)

-- TODO: replicate
-- TODO: replicateM

-------------------------------------------------------------------------------
-- Transformations
-------------------------------------------------------------------------------

map :: forall a b c m. (C a -> C b) -> StreamM m c a -> StreamM m c b
map f (MkStreamM s0 steps0) = MkStreamM s0 (go steps0) where
    go :: (SOP C xss -> (Step (C a) (SOP C xss) -> C r) -> C r)
       -> (SOP C xss -> (Step (C b) (SOP C xss) -> C r) -> C r)
    go steps s k = steps s $ \case
        Stop      -> k Stop
        Skip   s' -> k (Skip s')
        Emit a s' -> k (Emit (f a) s')

mapWithInput :: forall a b c m. (C a -> C b -> C c) -> StreamM m a b -> StreamM m a c
mapWithInput f (MkStreamM s0 steps0) =
    mkStreamM (\a -> (a, s0 a)) $ \(a, curr) k -> steps0 curr $ \case
        Stop        -> k Stop
        Skip   next -> k (Skip (a, next))
        Emit b next -> k (Emit (f a b) (a, next))

lmap :: forall a b c m. (C a -> C b) -> StreamM m b c -> StreamM m a c
lmap f (MkStreamM s0 steps0) = MkStreamM (s0 . f) steps0

traverse :: forall a b c m. Monad m => (C a -> C (m b)) -> StreamM m c a -> StreamM m c b
traverse f (MkStreamM s0 steps0) = MkStreamM s0 (go steps0) where
    go :: (SOP C xss -> (Step (C a) (SOP C xss) -> C (m r)) -> C (m r))
       -> (SOP C xss -> (Step (C b) (SOP C xss) -> C (m r)) -> C (m r))
    go steps s k = steps s $ \case
        Stop      -> k Stop
        Skip   s' -> k (Skip s')
        Emit a s' -> f a >>>= \a' -> k (Emit a' s')

-- TODO: traverseWithInput
-- TODO: ltraverse

filter :: forall a c m. (C a -> C Bool) -> StreamM m c a -> StreamM m c a
filter p (MkStreamM s0 steps0) = MkStreamM s0 (go steps0) where
    go :: (SOP C xss -> (Step (C a) (SOP C xss) -> C r) -> C r)
       -> (SOP C xss -> (Step (C a) (SOP C xss) -> C r) -> C r)
    go steps s k = steps s $ \case
        Stop      -> k Stop
        Skip   s' -> k (Skip s')
        Emit a s' -> sIfThenElse
            (p a)
            (k (Emit a s'))
            (k (Skip s'))

filterM :: forall a c m. Monad m => (C a -> C (m Bool)) -> StreamM m c a -> StreamM m c a
filterM p (MkStreamM s0 steps0) = MkStreamM s0 (go steps0) where
    go :: (SOP C xss -> (Step (C a) (SOP C xss) -> C (m r)) -> C (m r))
       -> (SOP C xss -> (Step (C a) (SOP C xss) -> C (m r)) -> C (m r))
    go steps s k = steps s $ \case
        Stop      -> k Stop
        Skip   s' -> k (Skip s')
        Emit a s' -> p a >>>= \pa -> sIfThenElse
            pa
            (k (Emit a s'))
            (k (Skip s'))

drop :: C Int -> StreamM m a b -> StreamM m a b
drop n (MkStreamM start steps) =
    mkStreamM (\a -> DropL n (start a)) $ \step k -> case step of
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

append :: forall a b m. StreamM m a b -> StreamM m a b -> StreamM m a b
append (MkStreamM startL stepsL) (MkStreamM startR stepsR) =
    mkStreamM (\a -> AppL a (startL a)) $ \step k -> case step of
        AppL a xss -> stepsL xss $ \case
            Stop        -> k (Skip   (AppR (startR a)))
            Skip   xss' -> k (Skip   (AppL a xss'))
            Emit b xss' -> k (Emit b (AppL a xss'))

        AppR yss -> stepsR yss $ \case
            Stop -> k Stop
            Skip   yss' -> k (Skip   (AppR yss'))
            Emit b yss' -> k (Emit b (AppR yss'))
        
empty :: StreamM m a b
empty = fromPure Pure.empty

-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

zipWith :: forall i a b c m. (C a -> C b -> C c) -> StreamM m i a -> StreamM m i b -> StreamM m i c
zipWith h (MkStreamM start0 steps0) (MkStreamM start1 steps1) =
    mkStreamM (\i -> ZipL (start0 i) (start1 i)) (steps steps0 steps1)
  where
    steps
        :: (forall r'. SOP C xss -> (Step (C a) (SOP C xss) -> C (m r')) -> C (m r'))
        -> (forall r'. SOP C yss -> (Step (C b) (SOP C yss) -> C (m r')) -> C (m r'))
        -> Zip a xss yss
        -> (Step (C c) (Zip a xss yss) -> C (m r))
        -> C (m r)
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
-- 'repeat' :: 'C' a -> 'StreamM' m i a
-- @
repeat :: IsCode a ca => ca -> StreamM m i a
repeat = fromPure . Pure.repeat

-- TODO: repeatM

-------------------------------------------------------------------------------
-- Recursion
-------------------------------------------------------------------------------

bfsTreeM
    :: forall a m. Monad m
    => StreamM m a a                -- ^ endo-stream
    -> (C a -> C (m Bool))  -- ^ whether to recurse on a produced element
    -> StreamM m a a
bfsTreeM (MkStreamM start0 steps0) p = mk start0 steps0 where
    mk  :: forall xss. SListI2 xss
        => (C a -> SOP C xss)
        -> (forall r. SOP C xss -> (Step (C a) (SOP C xss) -> C (m r)) -> C (m r))
        -> StreamM m a a
    mk start1 steps1 = mkStreamM (BfsNext . start1) steps2 where
        steps2  :: BFS a xss -> (Step (C a) (BFS a xss) -> C (m r)) -> C (m r)
        steps2 (BfsNext curr) k = steps1 curr $ \case
            Stop        -> k Stop
            Skip   next -> k (Skip (BfsNext next))
            Emit a next -> p a >>>= \b -> sIfThenElse b
                (k (Emit a (BfsStep (scons a snil) next)))
                (k (Emit a (BfsNext next)))

        steps2 (BfsStep as curr) k = steps1 curr $ \case
            Stop -> scaseList as
                (k Stop)
                (\a as' -> k (Skip (BfsStep as' (start1 a))))
            Skip   next -> k (Skip (BfsStep as next))
            Emit a next -> p a >>>= \b -> sIfThenElse b
                (k (Emit a (BfsStep (scons a as) next)))
                (k (Emit a (BfsStep as next)))

-------------------------------------------------------------------------------
-- Pipes
-------------------------------------------------------------------------------

idPipe :: StreamM m a a
idPipe = C.id

-- | Similar to 'map', prefer using 'map'.
mapPipe :: forall a b m. (C a -> C b) -> StreamM m a b
mapPipe f = mkStreamM (Just . f) step where
    step :: Maybe (C b) -> (Step (C b) (Maybe (C b)) -> C r) -> C r
    step Nothing  k = k Stop
    step (Just b) k = k (Emit b Nothing)

-- | Similar to 'traverse', prefer using 'traverse'.
traversePipe :: forall a b m. Monad m => (C a -> C (m b)) -> StreamM m a b
traversePipe f = mkStreamM (Just . f) step where
    step :: Maybe (C (m b)) -> (Step (C b) (Maybe (C (m b))) -> C (m r)) -> C (m r)
    step Nothing   k = k Stop
    step (Just mb) k = mb >>>= \b -> k (Emit b Nothing)

-- | Similar to 'filter', prefer using 'filter'.
filterPipe :: forall a m. (C a -> C Bool) -> StreamM m a a
filterPipe p = mkStreamM Just step where
    step :: Maybe (C a) -> (Step (C a) (Maybe (C a)) -> C r) -> C r
    step Nothing  k = k Stop
    step (Just a) k = sIfThenElse
        (p a)
        (k (Emit a Nothing))
        (k Stop) -- stop immediately, don't go to the second state.

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

run :: forall a b m. Monad m
    => C a -> StreamM m a b -> GHCCode (m ())
run z (MkStreamM start steps0) =
    fromCode $ sletrec_SOP (body steps0) (start z)
  where
    body
        :: (SOP C xss -> (Step (C b) (SOP C xss) -> C (m ())) -> C (m ()))
        -> (SOP C xss -> C (m ()))
        -> SOP C xss -> C (m ())
    body steps loop curr = steps curr $ \case
        Stop        -> sreturn sunit
        Skip   next -> loop next
        Emit _ next -> loop next

foldl :: forall r a b m. (Monad m)
      => (C r -> C b -> C r) -> C r -> C a -> StreamM m a b -> GHCCode (m r)
foldl op e z (MkStreamM xs steps0) =
    fromCode $ sletrec1_SOP (body steps0) (xs z) e
  where
    body
        :: (SOP C xss -> (Step (C b) (SOP C xss) -> C (m r)) -> C (m r))
        -> (SOP C xss -> C r -> C (m r))
        -> (SOP C xss -> C r -> C (m r))
    body steps loop curr acc = steps curr $ \case
        Stop        -> sreturn acc
        Skip   next -> loop next acc
        Emit b next -> loop next (op acc b)

foldlM
    :: forall r a b m. Monad m
    => (C r -> C b -> C (m r)) -> C r -> C a -> StreamM m a b -> GHCCode (m r)
foldlM op e z (MkStreamM xs steps0) =
    fromCode $ sletrec1_SOP (body steps0) (xs z) e
  where
    body
        :: (SOP C xss -> (Step (C b) (SOP C xss) -> C (m r)) -> C (m r))
        -> (SOP C xss -> C r -> C (m r))
        -> (SOP C xss -> C r -> C (m r))
    body steps loop curr acc = steps curr $ \case
        Stop        -> sreturn acc
        Skip   next -> loop next acc
        Emit b next -> op acc b >>>= loop next

toList
    :: forall a b m. Monad m
    => C a -> StreamM m a b -> GHCCode (m [b])
toList a (MkStreamM start steps0) =
    fromCode $ sletrec_SOP (body steps0) (start a)
  where
    body
        :: (SOP C xss -> (Step (C b) (SOP C xss) -> C (m [b])) -> C (m [b]))
        -> (SOP C xss -> C (m [b]))
        -> SOP C xss -> C (m [b])
    body steps loop curr = steps curr $ \case
        Stop        -> sreturn snil
        Skip   next -> loop next
        Emit b next -> sfmap (scons b) (loop next)
