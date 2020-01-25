{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Staged.Stream.ResourceT (
    bracket,
    ) where

import Data.Kind (Type)

import qualified GHC.Generics as GHC
import qualified Control.Monad.Trans.Resource as R

import Staged.Commons
import Staged.Stream
import Generics.SOP

bracket
    :: forall a b c m acquire release.
       (R.MonadResource m, Show a , ToCodeFn a (IO b) acquire, ToCodeFn b (IO ()) release)
    => acquire
    -> release
    -> StreamM m b c
    -> StreamM m a c
bracket alloc free (MkStreamM start0 steps0) =
    mk start0 steps0
  where
    mk :: forall xss. SListI2 xss
       => (C b -> SOP C xss)
       -> (forall r. SOP C xss -> (Step (C c) (SOP C xss) -> C (m r)) -> C (m r))
       -> StreamM m a c
    mk start1 steps1 = mkStreamM start2 steps2 where
        start2 :: C a -> Br a b xss
        start2 = Alloc

        steps2 :: Br a b xss -> (Step (C c) (Br a b xss) -> C (m r)) -> C (m r)
        steps2 (Alloc a) k = C [|| do
            p <- R.allocate $$(unC $ toFn alloc a) (\res -> $$(unC $ toFn free $ C [|| res ||]))
            $$(unC $ k (Skip (Inner (sfst $ C [|| p ||]) (start1 (ssnd $ C [|| p ||])))))
            ||]

        steps2 (Inner rk curr) k = steps1 curr $ \case
            Stop        -> C $ [|| do
                R.release $$(unC rk)
                $$(unC $ k Stop)
                ||]
            Skip   next -> k (Skip (Inner rk next))
            Emit c next -> k (Emit c (Inner rk next))

-- Bracket states:
--
-- * 'Alloc' saves the input. We cannot 'R.allocate' it directly,
--   as start function is not monadic.
--
-- * 'Inner' contains the state of inner stream and a 'ReleaseKey',
--   so we can /promptly/ 'R.release' the resource.
--
data Br a b (xss :: [[Type]])
    = Alloc (C a)
    | Inner (C R.ReleaseKey) (SOP C xss)
  deriving (GHC.Generic)

instance Generic (Br a b xss)
