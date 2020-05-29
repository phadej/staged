{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE EmptyCase               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- | More convenient (than direct) 'Stream' creation.
module Staged.Stream.Pure.Convenience (
    -- * Convenience helpers
    mkStream,
    -- * Constraint
    FlattenCode,
    ) where

import Data.SOP
import Staged.Commons
import Staged.Stream.Internal
import Staged.Stream.Pure.Type

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
--     start a = 'Just' ('C' (f a))
--
--     step :: 'Maybe' ('C' b) -> ('Step' ('C' b) ('Maybe' ('C' b)) -> 'C' r) -> 'C' r
--     step 'Nothing'      k = k 'Stop'
--     step ('Just' ('C' b)) k = k ('Emit' b 'Nothing')
-- @
--
-- /Note:/ you should prefer using 'Staged.Stream.Pure.Combinators.map' combinator if
-- possible, as it doesn't make state space bigger.
--
mkStream
    :: forall a b s xss. FlattenCode s xss
    => (C a -> s)                                             -- ^ start state
    -> (forall r. s -> (Step (C b) s -> C r) -> C r)  -- ^ step function
    -> Stream a b
mkStream start0 step0 =
    allFlattenCode (Proxy :: Proxy s) $ MkStream start step
  where
    start :: C a -> SOP C xss
    start = from' . start0

    step :: SOP C xss -> (Step (C b) (SOP C xss) -> C r) -> C r
    step s k = step0 (to' s) (k . fmap from')
