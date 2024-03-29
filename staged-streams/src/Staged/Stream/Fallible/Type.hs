{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Staged.Stream.Fallible.Type (
    StreamFM (..),
    FallibleStep (..),
    ) where

import Data.Kind (Type)
import Data.SOP  (NP (..), NS (..), SListI2, SOP (..))
import Data.Proxy (Proxy (..))

import qualified Control.Category as C

import Data.SOP.Fn.Append
import Data.SOP.Fn.ConcatMapAppend
import Staged.Commons
import Staged.Stream.States
import Staged.Stream.Fallible.Step

-------------------------------------------------------------------------------
-- Type definition
-------------------------------------------------------------------------------

-- | Pure staged streams.
--
-- Think of @'Stream' a b@ as @a -> [b]@, i.e.
-- a machine which will produce some @b@ values when triggered by single @a@ value.
--
data StreamFM :: (Type -> Type) -> Type -> Type -> Type where
    MkStreamFM
        :: SListI2 xss
        => (C a -> SOP C xss) -- ^ start function, initialize the inner state
        -> (forall r. SOP C xss -> (FallibleStep (C b) (SOP C xss) -> C (m r)) -> C (m r)) -- ^ step function
        -> StreamFM m a b

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance C.Category (StreamFM m) where
    id  = idStream
    (.) = flip composeStream

idStream :: forall a m. StreamFM m a a
idStream = MkStreamFM (\a -> SOP (Z (a :* Nil))) steps where
    steps :: SOP C '[ '[a], '[]] -> (FallibleStep (C a) (SOP C '[ '[a], '[]]) -> C r) -> C r
    steps (SOP ns) k = case ns of
        Z (a :* Nil) -> k (Emit a (SOP (S (Z Nil))))
        S (Z Nil)    -> k Stop
        S (S ns')    -> case ns' of {}

composeStream :: StreamFM m a b -> StreamFM m b c -> StreamFM m a c
composeStream (MkStreamFM x0 stepX) (MkStreamFM y0 stepY) = compose' x0 stepX y0 stepY

compose'
    :: forall a b c m xss yss. (SListI2 xss, SListI2 yss)
    => (C a -> SOP C xss)
    -> (forall r. SOP C xss -> (FallibleStep (C b) (SOP C xss) -> C (m r)) -> C (m r))
    -> (C b -> SOP C yss)
    -> (forall r. SOP C yss -> (FallibleStep (C c) (SOP C yss) -> C (m r)) -> C (m r))
    -> StreamFM m a c
compose' x0 stepsX0 y0 stepsY0
    = concatMapAppend_SListI2 prXss prYss
    $ append_SListI2 prXss (prConcatMapAppend prXss prYss)
    $ MkStreamFM z0 go0
  where
    prXss = Proxy :: Proxy xss
    prYss = Proxy :: Proxy yss

    z0 :: C a -> SOP C (Append xss (ConcatMapAppend xss yss))
    z0 = comp_SOP . CompL @xss @yss . x0

    go0 :: SOP C (Append xss (ConcatMapAppend xss yss)) -> (FallibleStep (C c) (SOP C (Append xss (ConcatMapAppend xss yss))) -> C (m r)) -> C (m r)
    go0 sop k = go1 (uncomp_SOP sop) $ k . fmap comp_SOP

    go1 :: Comp xss yss -> (FallibleStep (C c) (Comp xss yss) -> C (m r)) -> C (m r)
    go1 (CompL xss) k = stepsX0 xss $ \case
        Stop        -> k Stop
        Skip   xss' -> k (Skip (CompL xss'))
        Emit b xss' -> k (Skip (CompR xss' (y0 b)))
        Fail        -> k Fail

    go1 (CompR xss yss) k = stepsY0 yss $ \case
        Stop        -> k (Skip   (CompL xss))
        Skip   yss' -> k (Skip   (CompR xss yss'))
        Emit c yss' -> k (Emit c (CompR xss yss'))
        Fail        -> k Fail
