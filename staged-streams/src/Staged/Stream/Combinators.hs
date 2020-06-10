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
    iterateM,
    replicate,
    replicateM,
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
    zipWithM,
    repeat,
    repeatM,
    -- * Aligning
    alignWith,
    alignWithM,
    -- * Recursion
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

import Prelude (($), Bool(..), Either (..), Int, subtract, (<), (+), (.), Maybe (..), Monad, id)
-- import Prelude (undefined)

import Data.SOP (SOP (..), SListI2)
import Data.Functor.Identity (Identity (..))

import qualified Control.Category as  C

import Staged.Commons
import Staged.Stream.Step
import Staged.Stream.States
import Staged.Stream.Pure.Type
import Staged.Stream.Type
import Staged.Stream.Convenience
import Staged.Stream.Internal

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
    [|| runIdentity ||] @@ (steps curr $ \s -> toCode [|| Identity ||] @@ k s )

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- |
--
-- @
-- 'singleton' :: C b -> 'StreamM' m a b
-- 'singleton' b = 'map' ([|| const ||] @@ b)
-- @
singleton :: forall b a m cb. IsCode Q b cb => cb -> StreamM m a b
singleton = fromPure . Pure.singleton

-- |
--
-- @
-- 'fromList' :: (C a -> C [b]) -> 'StreamM' m a b
-- @
fromList :: ToCodeFn Q a [b] fn => fn -> StreamM m a b
fromList = fromPure . Pure.fromList

-- |
--
-- @
-- 'fromListM' :: (C a -> C (m [b])) -> 'StreamM' m a b
-- @
--
fromListM :: forall a b m fn. (Monad m, ToCodeFn Q a (m [b]) fn) => fn -> StreamM m a b
fromListM f = mkStreamM (Left . toFn f) steps where
    steps
        :: Either (C (m [b])) (C [b])
        -> (Step (C b) (Either (C (m [b])) (C [b])) -> C (m r))
        -> C (m r)
    steps (Left mbs) k = mbs >>>= \bs -> k (Skip (Right bs))
    steps (Right mb) k = scaseList mb
        (k Stop)
        (\b bs' -> k (Emit b (Right bs')))

-- |
--
-- Note: this can be used to unfold with monadic effects too.
--
-- @
-- 'unfold' :: (C a -> C b) -> (C b -> CPS (Maybe (C c, C b))) -> 'StreamM' m a c
-- @
unfold
    :: forall a b c m fn. ToCodeFn Q a b fn
    => fn
    -> (forall r. C b -> (Maybe (C c, C b) -> C (m r)) -> C (m r)) -- ^ unfolding
    -> StreamM m a c
unfold start f = mkStreamM (toFn start) steps where
    steps :: C b -> (Step (C c) (C b) -> C (m r)) -> C (m r)
    steps curr k = f curr $ \case
        Nothing        -> k Stop
        Just (c, next) -> k (Emit c next)

-- |
--
-- @
-- 'iterate' :: (C a -> C a) -> 'StreamM' m a a
-- @
iterate :: (ToCodeFn Q a a endo) => endo -> StreamM m a a
iterate f = fromPure (Pure.iterate f)

-- |
--
-- @
-- 'iterateM' :: (C a -> C (m a)) -> C a -> 'StreamM' m i a
-- @
iterateM :: (Monad m, ToCodeFn Q a (m a) endo) => endo -> StreamM m a a
iterateM f = mkStreamM id $ \curr k ->
    toFn f curr >>>= \x ->
    k (Emit curr x)

-- |
--
-- @
-- 'replicate' :: C Int -> C a -> 'Stream' i a
-- @
replicate :: (IsCode Q a ca, IsCode Q Int ci) => ci -> ca -> StreamM m i a
replicate i a = take i (repeat a)

-- |
--
-- @
-- 'replicate' :: C Int -> C a -> 'Stream' i a
-- @
replicateM :: (Monad m, IsCode Q (m a) ca, IsCode Q Int ci) => ci -> ca -> StreamM m i a
replicateM i a = take i (repeatM a)

-------------------------------------------------------------------------------
-- Transformations
-------------------------------------------------------------------------------

-- |
--
-- @
-- 'map' :: (C b -> C c) -> 'Stream' a b -> 'Stream' a c
-- @
map :: forall a b c m fn. ToCodeFn Q a b fn
    => fn -> StreamM m c a -> StreamM m c b
map f (MkStreamM s0 steps0) = MkStreamM s0 (go steps0) where
    go :: (SOP C xss -> (Step (C a) (SOP C xss) -> C r) -> C r)
       -> (SOP C xss -> (Step (C b) (SOP C xss) -> C r) -> C r)
    go steps s k = steps s $ \case
        Stop      -> k Stop
        Skip   s' -> k (Skip s')
        Emit a s' -> k (Emit (toFn f a) s')

-- |
--
-- @
-- 'mapWithInput' :: (C a -> C b -> C c) -> 'StreamM' m a b -> 'StreamM' m a c
-- @
mapWithInput
    :: forall a b c m fn. ToCodeFn2 Q a b c fn
    => fn -> StreamM m a b -> StreamM m a c
mapWithInput f (MkStreamM s0 steps0) =
    mkStreamM (\a -> (a, s0 a)) $ \(a, curr) k -> steps0 curr $ \case
        Stop        -> k Stop
        Skip   next -> k (Skip (a, next))
        Emit b next -> k (Emit (toFn2 f a b) (a, next))

-- |
--
-- @
-- 'lmap' :: (C a -> C b) -> 'StreamM' m b c -> 'StreamM' m a c
-- @
lmap
    :: forall a b c m fn. ToCodeFn Q a b fn
    => fn -> StreamM m b c -> StreamM m a c
lmap f (MkStreamM s0 steps0) = MkStreamM (s0 . toFn f) steps0

-- |
--
-- @
-- 'traverse' :: (C b -> C (m c)) -> 'StreamM' m a b -> 'StreamM' m a c
-- @
traverse
    :: forall a b c m fn. (Monad m, ToCodeFn Q b (m c) fn)
    => fn -> StreamM m a b -> StreamM m a c
traverse f (MkStreamM s0 steps0) = MkStreamM s0 (go steps0) where
    go :: (SOP C xss -> (Step (C b) (SOP C xss) -> C (m r)) -> C (m r))
       -> (SOP C xss -> (Step (C c) (SOP C xss) -> C (m r)) -> C (m r))
    go steps s k = steps s $ \case
        Stop      -> k Stop
        Skip   s' -> k (Skip s')
        Emit a s' -> toFn f a >>>= \a' -> k (Emit a' s')

-- TODO: traverseWithInput
-- TODO: ltraverse

-- |
--
-- @
-- 'filter' :: (C a -> C Bool) -> 'StreamM' m c a -> 'StreamM' m c a
-- @
filter
    :: forall a c m predicate. (ToCodeFn Q a Bool predicate)
    => predicate -> StreamM m c a -> StreamM m c a
filter p (MkStreamM s0 steps0) = MkStreamM s0 (go steps0) where
    go :: (SOP C xss -> (Step (C a) (SOP C xss) -> C r) -> C r)
       -> (SOP C xss -> (Step (C a) (SOP C xss) -> C r) -> C r)
    go steps s k = steps s $ \case
        Stop      -> k Stop
        Skip   s' -> k (Skip s')
        Emit a s' -> sIfThenElse
            (toFn p a)
            (k (Emit a s'))
            (k (Skip s'))

-- |
--
-- @
-- 'filterM' :: (C a -> C (m Bool)) -> 'StreamM' m c a -> 'StreamM' m c a
-- @
filterM
    :: forall a c m predicateM. (Monad m, ToCodeFn Q a (m Bool) predicateM)
    => predicateM -> StreamM m c a -> StreamM m c a
filterM p (MkStreamM s0 steps0) = MkStreamM s0 (go steps0) where
    go :: (SOP C xss -> (Step (C a) (SOP C xss) -> C (m r)) -> C (m r))
       -> (SOP C xss -> (Step (C a) (SOP C xss) -> C (m r)) -> C (m r))
    go steps s k = steps s $ \case
        Stop      -> k Stop
        Skip   s' -> k (Skip s')
        Emit a s' -> toFn p a >>>= \pa -> sIfThenElse
            pa
            (k (Emit a s'))
            (k (Skip s'))

-------------------------------------------------------------------------------
-- Take and drop
-------------------------------------------------------------------------------

-- |
--
-- @
-- 'take' :: C Int -> 'StreamM' m a b -> 'StreamM' m a b
-- @
take :: IsCode Q Int n => n -> StreamM m a b -> StreamM m a b
take n (MkStreamM start steps) = do
    mkStreamM (\a -> (toCode [|| 0 ||], start a)) $ \(i, xss) k -> steps xss $ \case
        Stop        -> k Stop
        Skip   xss' -> k (Skip (i, xss'))
        Emit b xss' -> sIfThenElse
            (toCode [|| (<) ||] @@ i @@ toCode n)
            (k (Emit b (toCode [|| (1 +) ||] @@ i, xss')))
            (k Stop)
-- |
--
-- @
-- 'drop' :: C Int -> 'StreamM' m a b -> 'StreamM' m a b
-- @
drop :: IsCode Q Int n => n -> StreamM m a b -> StreamM m a b
drop n (MkStreamM start steps) =
    mkStreamM (\a -> DropL (toCode n) (start a)) $ \step k -> case step of
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

-- |
--
-- @
-- 'zipWith' :: (C a -> C b -> C c) -> 'StreamM' m i a -> 'StreamM' m i b -> 'StreamM' m i c
-- @
zipWith :: forall i a b c m abc. ToCodeFn2 Q a b c abc => abc -> StreamM m i a -> StreamM m i b -> StreamM m i c
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
        Skip   yss' -> k (Skip               (ZipR a xss yss'))
        Emit b yss' -> k (Emit (toFn2 h a b) (ZipL xss yss'))

-- |
--
-- @
-- 'zipWithM' :: (C a -> C b -> C (m c)) -> 'StreamM' m i a -> 'StreamM' m i b -> 'StreamM' m i c
-- @
zipWithM
    :: forall i a b c m abc. (Monad m, ToCodeFn2 Q a b (m c) abc)
    => abc -> StreamM m i a -> StreamM m i b -> StreamM m i c
zipWithM h (MkStreamM start0 steps0) (MkStreamM start1 steps1) =
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
        Skip   yss' -> k (Skip (ZipR a xss yss'))
        Emit b yss' -> toFn2 h a b >>>= \c -> k (Emit c (ZipL xss yss'))


-- |
--
-- @
-- 'repeat' :: 'C' a -> 'StreamM' m i a
-- @
repeat :: IsCode Q a ca => ca -> StreamM m i a
repeat = fromPure . Pure.repeat

-- |
--
-- @
-- 'repeatM' :: 'C' (m a) -> 'StreamM' m i a
-- @
repeatM :: (Monad m, IsCode Q (m a) cma) => cma -> StreamM m i a
repeatM a = mkStreamM (\_ -> ()) $ \() k -> toCode a >>>= \a' -> k (Emit a' ())

-------------------------------------------------------------------------------
-- Aligning
-------------------------------------------------------------------------------

-- |
--
-- @
-- 'alignWith' :: (C a -> C c) -> (C b -> C c) -> (C a -> C b -> C c)
--             -> 'StreamM' i a -> 'StreamM' i b -> 'StreamM' i c
-- @
alignWith
    :: forall i a b c m ac bc abc. (ToCodeFn Q a c ac, ToCodeFn Q b c bc, ToCodeFn2 Q a b c abc)
    => ac -> bc -> abc
    -> StreamM m i a -> StreamM m i b -> StreamM m i c
alignWith ac bc abc (MkStreamM start0 steps0) (MkStreamM start1 steps1) =
    mkStreamM (\i -> AlignL (start0 i) (start1 i)) (steps steps0 steps1)
  where
    steps
        :: (forall r'. SOP C xss -> (Step (C a) (SOP C xss) -> C (m r')) -> C (m r'))
        -> (forall r'. SOP C yss -> (Step (C b) (SOP C yss) -> C (m r')) -> C (m r'))
        -> Align a xss yss
        -> (Step (C c) (Align a xss yss) -> C (m r))
        -> C (m r)
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

-- |
--
-- @
-- 'alignWith' :: (C a -> C (m c)) -> (C b -> C (m c)) -> (C a -> C b -> C (m c))
--             -> 'StreamM' i a -> 'StreamM' i b -> 'StreamM' i c
-- @
alignWithM
    :: forall i a b c m ac bc abc. (Monad m, ToCodeFn Q a (m c) ac, ToCodeFn Q b (m c) bc, ToCodeFn2 Q a b (m c) abc)
    => ac -> bc -> abc
    -> StreamM m i a -> StreamM m i b -> StreamM m i c
alignWithM ac bc abc (MkStreamM start0 steps0) (MkStreamM start1 steps1) =
    mkStreamM (\i -> AlignL (start0 i) (start1 i)) (steps steps0 steps1)
  where
    steps
        :: (forall r'. SOP C xss -> (Step (C a) (SOP C xss) -> C (m r')) -> C (m r'))
        -> (forall r'. SOP C yss -> (Step (C b) (SOP C yss) -> C (m r')) -> C (m r'))
        -> Align a xss yss
        -> (Step (C c) (Align a xss yss) -> C (m r))
        -> C (m r)
    steps f _ (AlignL xss yss) k = f xss $ \case
        Stop        -> k (Skip (AlignDrainR yss))
        Skip   xss' -> k (Skip (AlignL xss' yss))
        Emit a xss' -> k (Skip (AlignR a xss' yss))

    steps _ g (AlignR a xss yss) k = g yss $ \case
        Stop        -> toFn ac a     >>>= \c -> k (Emit c (AlignDrainL xss))
        Skip   yss' ->                          k (Skip   (AlignR a xss yss'))
        Emit b yss' -> toFn2 abc a b >>>= \c -> k (Emit c (AlignL xss yss'))

    steps f _ (AlignDrainL xss) k = f xss $ \case
        Stop        ->                      k Stop
        Skip   xss' ->                      k (Skip   (AlignDrainL xss'))
        Emit a xss' -> toFn ac a >>>= \c -> k (Emit c (AlignDrainL xss'))

    steps _ g (AlignDrainR yss) k = g yss $ \case
        Stop        ->                      k Stop
        Skip   yss' ->                      k (Skip   (AlignDrainR yss'))
        Emit b yss' -> toFn bc b >>>= \c -> k (Emit c (AlignDrainR yss'))

-------------------------------------------------------------------------------
-- Recursion
-------------------------------------------------------------------------------

-- |
--
-- @
-- 'bfsTreeM' :: 'StreamM' m a a -> (C a -> C (m Bool)) -> 'StreamM' m a a
-- @
bfsTreeM
    :: forall a m predicateM. (Monad m, ToCodeFn Q a (m Bool) predicateM)
    => StreamM m a a  -- ^ endo-stream
    -> predicateM     -- ^ whether to recurse on a produced element
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
            Emit a next -> toFn p a >>>= \b -> sIfThenElse b
                (k (Emit a (BfsStep (scons a snil) next)))
                (k (Emit a (BfsNext next)))

        steps2 (BfsStep as curr) k = steps1 curr $ \case
            Stop -> scaseList as
                (k Stop)
                (\a as' -> k (Skip (BfsStep as' (start1 a))))
            Skip   next -> k (Skip (BfsStep as next))
            Emit a next -> toFn p a >>>= \b -> sIfThenElse b
                (k (Emit a (BfsStep (scons a as) next)))
                (k (Emit a (BfsStep as next)))

-------------------------------------------------------------------------------
-- Pipes
-------------------------------------------------------------------------------

-- | Identity stream.
idPipe :: StreamM m a a
idPipe = C.id

-- | Similar to 'map', prefer using 'map'.
--
-- @
-- 'mapPipe' :: (C a -> C b) -> 'StreamM' m a b
-- @
mapPipe :: forall a b m fn. ToCodeFn Q a b fn => fn -> StreamM m a b
mapPipe f = mkStreamM (Just . toFn f) step where
    step :: Maybe (C b) -> (Step (C b) (Maybe (C b)) -> C r) -> C r
    step Nothing  k = k Stop
    step (Just b) k = k (Emit b Nothing)

-- | Similar to 'traverse', prefer using 'traverse'.
--
-- @
-- 'traversePipe' :: (C a -> C (m b)) -> 'StreamM' m a b
-- @
traversePipe :: forall a b m fn. (Monad m, ToCodeFn Q a (m b) fn) => fn -> StreamM m a b
traversePipe f = mkStreamM (Just . toFn f) step where
    step :: Maybe (C (m b)) -> (Step (C b) (Maybe (C (m b))) -> C (m r)) -> C (m r)
    step Nothing   k = k Stop
    step (Just mb) k = mb >>>= \b -> k (Emit b Nothing)

-- | Similar to 'filter', prefer using 'filter'.
--
-- @
-- 'filterPipe' :: (C a -> C Bool) -> 'StreamM' m a a
-- @
filterPipe :: forall a m predicate. ToCodeFn Q a Bool predicate => predicate -> StreamM m a a
filterPipe p = mkStreamM Just step where
    step :: Maybe (C a) -> (Step (C a) (Maybe (C a)) -> C r) -> C r
    step Nothing  k = k Stop
    step (Just a) k = sIfThenElse
        (toFn p a)
        (k (Emit a Nothing))
        (k Stop) -- stop immediately, don't go to the second state.

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-- |
--
-- @
-- 'run' :: C a -> 'StreamM' m a b -> GHCCode (m ())
-- @
run :: forall a b m ca. (Monad m, IsCode Q a ca)
    => ca -> StreamM m a b -> GHCCode (m ())
run z (MkStreamM start steps0) =
    fromCode $ sletrec_SOP (body steps0) (start (toCode z))
  where
    body
        :: (SOP C xss -> (Step (C b) (SOP C xss) -> C (m ())) -> C (m ()))
        -> (SOP C xss -> C (m ()))
        -> SOP C xss -> C (m ())
    body steps loop curr = steps curr $ \case
        Stop        -> sreturn sunit
        Skip   next -> loop next
        Emit _ next -> loop next

-- |
--
-- @
-- 'foldl' :: (C r -> C b -> C r) -> C r -> C a -> StreamM m a b -> GHCCode (m r)
-- @
foldl :: forall r a b m fn init start. (Monad m, IsCode Q r init, IsCode Q a start, ToCodeFn2 Q r b r fn)
      => fn -> init -> start -> StreamM m a b -> GHCCode (m r)
foldl op e z (MkStreamM xs steps0) =
    fromCode $ sletrec1_SOP (body steps0) (xs (toCode z)) (toCode e)
  where
    body
        :: (SOP C xss -> (Step (C b) (SOP C xss) -> C (m r)) -> C (m r))
        -> (SOP C xss -> C r -> C (m r))
        -> (SOP C xss -> C r -> C (m r))
    body steps loop curr acc = steps curr $ \case
        Stop        -> sreturn acc
        Skip   next -> loop next acc
        Emit b next -> loop next (toFn2 op acc b)

-- |
--
-- @
-- 'foldlM' :: (C r -> C b -> C (m r)) -> C r -> C a -> StreamM m a b -> GHCCode (m r)
-- @
foldlM
    :: forall r a b m fn init start. (Monad m, IsCode Q r init, IsCode Q a start, ToCodeFn2 Q r b (m r) fn)
    => fn -> init -> start -> StreamM m a b -> GHCCode (m r)
foldlM op e z (MkStreamM xs steps0) =
    fromCode $ sletrec1_SOP (body steps0) (xs (toCode z)) (toCode e)
  where
    body
        :: (SOP C xss -> (Step (C b) (SOP C xss) -> C (m r)) -> C (m r))
        -> (SOP C xss -> C r -> C (m r))
        -> (SOP C xss -> C r -> C (m r))
    body steps loop curr acc = steps curr $ \case
        Stop        -> sreturn acc
        Skip   next -> loop next acc
        Emit b next -> toFn2 op acc b >>>= loop next

-- |
--
-- @
-- 'toList' :: C a -> StreamM a b -> GHCCode (m [b])
-- @
toList
    :: forall a b m ca. (Monad m, IsCode Q a ca)
    => ca -> StreamM m a b -> GHCCode (m [b])
toList a (MkStreamM start steps0) =
    fromCode $ sletrec_SOP (body steps0) (start (toCode a))
  where
    body
        :: (SOP C xss -> (Step (C b) (SOP C xss) -> C (m [b])) -> C (m [b]))
        -> (SOP C xss -> C (m [b]))
        -> SOP C xss -> C (m [b])
    body steps loop curr = steps curr $ \case
        Stop        -> sreturn snil
        Skip   next -> loop next
        Emit b next -> sfmap (scons b) (loop next)
