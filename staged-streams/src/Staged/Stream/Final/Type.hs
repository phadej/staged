{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
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
module Staged.Stream.Final.Type (
    Stream (..),
    Step (..),
    ) where

import Data.Kind  (Type)
import Data.Proxy (Proxy (..))
import Data.SOP   (NP (..), NS (..), SListI2, SOP (..))

import qualified Control.Category as C

import Data.SOP.Fn.Append
import Data.SOP.Fn.ConcatMapAppend
import Staged.Stream.Step
import Staged.Stream.Final.States

-------------------------------------------------------------------------------
-- Type definition
-------------------------------------------------------------------------------

-- | Pure staged streams.
--
-- Think of @'Stream' a b@ as @a -> [b]@, i.e.
-- a machine which will produce some @b@ values when triggered by single @a@ value.
--
data Stream :: (k -> Type) -> k -> k -> Type where
    MkStream
        :: SListI2 xss
        => (code a -> SOP code xss) -- ^ start function, initialize the inner state
        -> (forall r. SOP code xss -> (Step (code b) (SOP code xss) -> code r) -> code r) -- ^ step function
        -> Stream code a b

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance C.Category (Stream code) where
    id  = idStream
    (.) = flip composeStream

idStream :: forall code a. Stream code a a
idStream = MkStream (\a -> SOP (Z (a :* Nil))) steps where
    steps :: SOP code '[ '[a], '[]] -> (Step (code a) (SOP code '[ '[a], '[]]) -> code r) -> code r
    steps (SOP ns) k = case ns of
        Z (a :* Nil) -> k (Emit a (SOP (S (Z Nil))))
        S (Z Nil)    -> k Stop
        S (S ns')    -> case ns' of {}

composeStream :: Stream code a b -> Stream code b c -> Stream code a c
composeStream (MkStream x0 stepX) (MkStream y0 stepY) = compose' x0 stepX y0 stepY

compose'
    :: forall code a b c xss yss. (SListI2 xss, SListI2 yss)
    => (code a -> SOP code xss)
    -> (forall r. SOP code xss -> (Step (code b) (SOP code xss) -> code r) -> code r)
    -> (code b -> SOP code yss)
    -> (forall r. SOP code yss -> (Step (code c) (SOP code yss) -> code r) -> code r)
    -> Stream code a c
compose' x0 stepsX0 y0 stepsY0
    = concatMapAppend_SListI2 prXss prYss
    $ append_SListI2 prXss (prConcatMapAppend prXss prYss)
    $ MkStream z0 go0
  where
    prXss = Proxy :: Proxy xss
    prYss = Proxy :: Proxy yss

    z0 :: code a -> SOP code (Append xss (ConcatMapAppend xss yss))
    z0 = comp_SOP . CompL @xss @yss @code . x0

    go0 :: SOP code (Append xss (ConcatMapAppend xss yss)) -> (Step (code c) (SOP code (Append xss (ConcatMapAppend xss yss))) -> code r) -> code r
    go0 sop k = go1 (uncomp_SOP sop) $ k . fmap comp_SOP

    go1 :: Comp xss yss code -> (Step (code c) (Comp xss yss code) -> code r) -> code r
    go1 (CompL xss) k = stepsX0 xss $ \case
        Stop        -> k Stop
        Skip   xss' -> k (Skip (CompL xss'))
        Emit b xss' -> k (Skip (CompR xss' (y0 b)))

    go1 (CompR xss yss) k = stepsY0 yss $ \case
        Stop        -> k (Skip   (CompL xss))
        Skip   yss' -> k (Skip   (CompR xss yss'))
        Emit c yss' -> k (Emit c (CompR xss yss'))
