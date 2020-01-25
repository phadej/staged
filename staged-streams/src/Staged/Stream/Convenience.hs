{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
-- | More convenient (than direct) 'StreamM' creation.
module Staged.Stream.Convenience (
    -- * Convenience helpers:
    mkStreamM,
    -- * Type families
    FlattenCode, FlattenCode0, FlattenCode1, FlattenCode2,
    FlattenCodeErr, FlattenCodeErr0, FlattenCodeErr1, FlattenCodeErr2,
    ) where

import Generics.SOP (Generic, Proxy (..), SOP (..), from, to, unSOP)

import Data.SOP.Fn.All
import Data.SOP.Fn.Flatten
import Staged.Commons
import Staged.Stream.Pure.Convenience
       (FlattenCode, FlattenCode0 (..), FlattenCode1, FlattenCode2,
       FlattenCodeErr, FlattenCodeErr0, FlattenCodeErr1, FlattenCodeErr2)
import Staged.Stream.Type

-- | Create 'StreamM' from a simple state.
--
-- __Example:__ definition of 'Staged.Stream.Combinators.traversePipe'.
-- There are two states: the initial one and the "stopped" one, which
-- stream moves into after yielding a value.
-- This state space is representable with 'Maybe':
--
-- @
-- 'Staged.Stream.Combinators.traversePipe' :: forall a b m. 'Monad' m => ('C' a -> 'C' (m b)) -> 'StreamM' m a b
-- 'Staged.Stream.Combinators.traversePipe' f = 'mkStreamM' start step where
--     start :: 'C' a -> 'Maybe' ('C' (m b))
--     start a = 'Just' ('C' (f a))
--
--     step :: 'Maybe' ('C' (m b)) -> ('Step' ('C' b) ('Maybe' ('C' (m b))) -> 'C' (m r)) -> 'C' (m r)
--     step 'Nothing'       k = k 'Stop'
--     step ('Just' ('C' mb)) k = mb '>>>=' \\b -> k ('Emit' b 'Nothing')
-- @
--
-- /Note:/ you should prefer using 'Staged.Stream.Pure.Combinators.map' combinator if
-- possible, as it doesn't make state space bigger.
--
mkStreamM
    :: forall a b s m xssss. (Generic s, FlattenCodeErr s, FlattenCode s xssss)
    => (C a -> s)                                                     -- ^ start state
    -> (forall r. s -> (Step (C b) s -> C (m r)) -> C (m r))  -- ^ step function
    -> StreamM m a b
mkStreamM start0 step0 =
    allFLATTEN prTop (Proxy :: Proxy xssss) $ MkStreamM start step
  where
    start :: C a -> SOP C (FLATTEN xssss)
    start = from' . start0

    step :: SOP C (FLATTEN xssss) -> (Step (C b) (SOP C (FLATTEN xssss)) -> C (m r)) -> C (m r)
    step s k = step0 (to' s) (k . fmap from')

    from' :: s -> SOP C (FLATTEN xssss)
    from' = SOP . flatten_NSNP . codeFlatFwd . unSOP . from

    to' :: SOP C (FLATTEN xssss) -> s
    to' = to . SOP . codeFlatBwd . unflatten_NSNP . unSOP
