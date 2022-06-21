{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE EmptyCase               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- | More convenient (than direct) 'Stream' creation.
module Staged.Stream.Final.Convenience (
    -- * Convenience helpers
    mkStreamG,
    -- * Constraint
    FlattenCode,
    ) where

import Data.SOP
import Data.Kind (Type)

import Staged.Stream.Final.Internal
import Staged.Stream.Final.Type
import Staged.Stream.Final.States

-- | Create 'Stream' from a simple state.
--
-- __Example:__ definition of 'Staged.Stream.Pure.Combinators.mapPipe'.
-- There are two states: the initial one and the "stopped" one, which
-- stream moves into after yielding a value.
-- This state space is representable with 'Maybe':
--
-- @
-- 'Staged.Stream.Pure.Combinators.mapPipe' :: forall a b. ('C' a -> 'C' b) -> 'Stream' a b
-- 'Staged.Stream.Pure.Combinators.mapPipe' f = 'mkStream' start step where
--     start :: 'C' a -> 'Maybe' ('C' b)
--     start a = 'Just' (f a)
--
--     step :: 'Maybe' ('C' b) -> ('Step' ('C' b) ('Maybe' ('C' b)) -> 'C' r) -> 'C' r
--     step 'Nothing'  k = k 'Stop'
--     step ('Just' b) k = k ('Emit' b 'Nothing')
-- @
--
-- /Note:/ you should prefer using 'Staged.Stream.Combinators.traverse' combinator if
-- possible, as it doesn't make state space bigger.
--
mkStreamG
    :: forall {k} (code :: k -> Type) (a :: k) (b :: k) (s :: (k -> Type) -> Type) (xss :: [[k]]). FlattenCode s code xss
    => (code a -> s code)                                                  -- ^ start state
    -> (forall r. s code -> (Step (code b) (s code) -> code r) -> code r)  -- ^ step function
    -> StreamG code a b
mkStreamG start0 step0 =
    allFlattenCode (Proxy @s) (Proxy @code) $ MkStreamG start step
  where
    start :: code a -> SOP code xss
    start = from' . start0

    step :: SOP code xss -> (Step (code b) (SOP code xss) -> code r) -> code r
    step s k = step0 (to' s) (k . fmap from')

-------------------------------------------------------------------------------
-- test: WIP
-------------------------------------------------------------------------------

{-
append :: forall code a b. StreamG code a b -> StreamG code a b -> StreamG code a b
append (MkStreamG startL stepsL) (MkStreamG startR stepsR) =
    mkStreamG (\a -> AppL (O a) (startL a)) $ \step k -> case step of
        AppL a xss -> stepsL xss $ \case
            Stop        -> k (Skip   (AppR (startR (unO a))))
            Skip   xss' -> k (Skip   (AppL a xss'))
            Emit b xss' -> k (Emit b (AppL a xss'))

        AppR yss -> stepsR yss $ \case
            Stop -> k Stop
            Skip   yss' -> k (Skip   (AppR yss'))
            Emit b yss' -> k (Emit b (AppR yss'))
-}
