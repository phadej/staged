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
    StreamG (..),
    Step (..),
    ) where

import Data.Kind  (Type)
import Data.Proxy (Proxy (..))
import Data.SOP   (NP (..), NS (..), SListI2, SOP (..))

import qualified Control.Category as C
import qualified GHC.Generics     as GHC

import Data.SOP.Fn.Append
import Data.SOP.Fn.ConcatMapAppend
import Staged.Stream.Step

-------------------------------------------------------------------------------
-- Type definition
-------------------------------------------------------------------------------

-- | Pure staged streams.
--
-- Think of @'Stream' a b@ as @a -> [b]@, i.e.
-- a machine which will produce some @b@ values when triggered by single @a@ value.
--
data StreamG :: (k -> Type) -> k -> k -> Type where
    MkStreamG
        :: SListI2 xss
        => (code a -> SOP code xss) -- ^ start function, initialize the inner state
        -> (forall r. SOP code xss -> (Step (code b) (SOP code xss) -> code r) -> code r) -- ^ step function
        -> StreamG code a b

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance C.Category (StreamG code) where
    id  = idStream
    (.) = flip composeStream

idStream :: forall code a. StreamG code a a
idStream = MkStreamG (\a -> SOP (Z (a :* Nil))) steps where
    steps :: SOP code '[ '[a], '[]] -> (Step (code a) (SOP code '[ '[a], '[]]) -> code r) -> code r
    steps (SOP ns) k = case ns of
        Z (a :* Nil) -> k (Emit a (SOP (S (Z Nil))))
        S (Z Nil)    -> k Stop
        S (S ns')    -> case ns' of {}

composeStream :: StreamG code a b -> StreamG code b c -> StreamG code a c
composeStream (MkStreamG x0 stepX) (MkStreamG y0 stepY) = compose' x0 stepX y0 stepY

compose'
    :: forall code a b c xss yss. (SListI2 xss, SListI2 yss)
    => (code a -> SOP code xss)
    -> (forall r. SOP code xss -> (Step (code b) (SOP code xss) -> code r) -> code r)
    -> (code b -> SOP code yss)
    -> (forall r. SOP code yss -> (Step (code c) (SOP code yss) -> code r) -> code r)
    -> StreamG code a c
compose' x0 stepsX0 y0 stepsY0
    = concatMapAppend_SListI2 prXss prYss
    $ append_SListI2 prXss (prConcatMapAppend prXss prYss)
    $ MkStreamG z0 go0
  where
    prXss = Proxy :: Proxy xss
    prYss = Proxy :: Proxy yss

    z0 :: code a -> SOP code (Append xss (ConcatMapAppend xss yss))
    z0 = comp_SOP . CompL @code @xss @yss . x0

    go0 :: SOP code (Append xss (ConcatMapAppend xss yss)) -> (Step (code c) (SOP code (Append xss (ConcatMapAppend xss yss))) -> code r) -> code r
    go0 sop k = go1 (uncomp_SOP sop) $ k . fmap comp_SOP

    go1 :: Comp code xss yss -> (Step (code c) (Comp code xss yss) -> code r) -> code r
    go1 (CompL xss) k = stepsX0 xss $ \case
        Stop        -> k Stop
        Skip   xss' -> k (Skip (CompL xss'))
        Emit b xss' -> k (Skip (CompR xss' (y0 b)))

    go1 (CompR xss yss) k = stepsY0 yss $ \case
        Stop        -> k (Skip   (CompL xss))
        Skip   yss' -> k (Skip   (CompR xss yss'))
        Emit c yss' -> k (Emit c (CompR xss yss'))

-------------------------------------------------------------------------------
-- Composition
-------------------------------------------------------------------------------

-- TODO: copy of what is in Staged.Streams.States

data Comp code xss yss
    = CompL (SOP code xss)
    | CompR (SOP code xss) (SOP code yss)
  deriving (GHC.Generic)

comp_SOP
    :: forall code xss yss. (SListI2 xss, SListI2 yss)
    => Comp code xss yss
    -> SOP code (Append xss (ConcatMapAppend xss yss))
comp_SOP (CompL (SOP xss)) = SOP (injLeft (prConcatMapAppend prXss prYss) xss)
  where
    prXss = Proxy :: Proxy xss
    prYss = Proxy :: Proxy yss

comp_SOP (CompR (SOP xss) (SOP yss))
    = SOP
    $ injRight prXss
    $ concatMapAppend_NSNP xss yss
  where
    prXss = Proxy :: Proxy xss

uncomp_SOP
    :: forall code xss yss. (SListI2 xss, SListI2 yss)
    => SOP code (Append xss (ConcatMapAppend xss yss))
    -> Comp code xss yss
uncomp_SOP (SOP sop) = case split_NS' prXss (prConcatMapAppend prXss prYss) sop of
    Left xss   -> CompL (SOP xss)
    Right sop' -> case unconcatMapAppend_NSNP sop' of
        (xss, yss) -> CompR (SOP xss) (SOP yss)
  where
    prXss = Proxy :: Proxy xss
    prYss = Proxy :: Proxy yss
