{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Staged.Stream.Pure.Type (
    Stream (..),
    Step (..),
    ) where

import Data.Kind (Type)
import Generics.SOP

import qualified Control.Category as C

import Staged.Commons
import Staged.Stream.Step
import Staged.Stream.States
import Data.SOP.Fn.Append
import Data.SOP.Fn.ConcatMapAppend

-------------------------------------------------------------------------------
-- Type definition
-------------------------------------------------------------------------------

data Stream :: Type -> Type -> Type where
    MkStream
        :: SListI2 xss
        => (C a -> SOP C xss) -- ^ start function, initialize the inner state
        -> (forall r. SOP C xss -> (Step (C b) (SOP C xss) -> C r) -> C r) -- ^ step function
        -> Stream a b

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance C.Category Stream where
    id  = idStream
    (.) = flip composeStream

idStream :: forall a. Stream a a
idStream = MkStream (\a -> SOP (Z (a :* Nil))) steps where
    steps :: SOP C '[ '[a], '[]] -> (Step (C a) (SOP C '[ '[a], '[]]) -> C r) -> C r
    steps (SOP ns) k = case ns of
        Z (a :* Nil) -> k (Emit a (SOP (S (Z Nil))))
        S (Z Nil)    -> k Stop
        S (S ns')    -> case ns' of {}

composeStream :: Stream a b -> Stream b c -> Stream a c
composeStream (MkStream x0 stepX) (MkStream y0 stepY) = compose' x0 stepX y0 stepY

compose'
    :: forall a b c xss yss. (SListI2 xss, SListI2 yss)
    => (C a -> SOP C xss)
    -> (forall r. SOP C xss -> (Step (C b) (SOP C xss) -> C r) -> C r)
    -> (C b -> SOP C yss)
    -> (forall r. SOP C yss -> (Step (C c) (SOP C yss) -> C r) -> C r)
    -> Stream a c
compose' x0 stepsX0 y0 stepsY0
    = concatMapAppend_SListI2 prXss prYss
    $ append_SListI2 prXss (prConcatMapAppend prXss prYss)
    $ MkStream z0 go0
  where
    prXss = Proxy :: Proxy xss
    prYss = Proxy :: Proxy yss

    z0 :: C a -> SOP C (Append xss (ConcatMapAppend xss yss))
    z0 = comp_SOP . CompL @xss @yss . x0

    go0 :: SOP C (Append xss (ConcatMapAppend xss yss)) -> (Step (C c) (SOP C (Append xss (ConcatMapAppend xss yss))) -> C r) -> C r
    go0 sop k = go1 (uncomp_SOP sop) $ k . fmap comp_SOP

    go1 :: Comp xss yss -> (Step (C c) (Comp xss yss) -> C r) -> C r
    go1 (CompL xss) k = stepsX0 xss $ \case
        Stop        -> k Stop
        Skip   xss' -> k (Skip (CompL xss'))
        Emit b xss' -> k (Skip (CompR xss' (y0 b)))

    go1 (CompR xss yss) k = stepsY0 yss $ \case
        Stop        -> k (Skip   (CompL xss))
        Skip   yss' -> k (Skip   (CompR xss yss'))
        Emit c yss' -> k (Emit c (CompR xss yss'))
