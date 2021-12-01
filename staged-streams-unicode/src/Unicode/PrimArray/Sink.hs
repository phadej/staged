{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Unicode.PrimArray.Sink (
    sinkPrimArray,
) where

import Data.SOP                    (SOP)
import Staged.Stream.Fallible.Step (FallibleStep (..))

import Staged.Commons

import qualified Control.Monad.Primitive as P
import qualified Data.Primitive          as P
import qualified Staged.Stream.Fallible  as S

sinkPrimArray
    :: forall a p r. P.Prim p
    => C (IO r)                                  -- ^ on error
    -> C Int                                     -- ^ maximum length to allocate
    -> C a                                       -- ^ start element
    -> S.StreamFM IO a p                         -- ^ stream to P
    -> (C Int -> C (P.PrimArray p) -> C (IO r))  -- ^ continuation with element count and primarray.
    -> C (IO r)
sinkPrimArray err len a (S.MkStreamFM start steps0) k = [|| do
    arr <- P.newPrimArray $$len
    $$(S.sletrec1_SOP (body [|| arr ||] steps0) (start a) (sint 0))
 ||]
  where
    body
        :: C (P.MutablePrimArray (P.PrimState IO) p)
        -> (SOP C xss -> (FallibleStep (C p) (SOP C xss) -> C (IO r)) -> C (IO r))
        -> (SOP C xss -> C Int -> C (IO r))
        -> (SOP C xss -> C Int -> C (IO r))
    body arr steps loop curr idx = steps curr $ \case
        Fail        -> err
        Stop        -> [|| do { P.shrinkMutablePrimArray $$arr $$idx; P.unsafeFreezePrimArray $$arr } ||] >>>= k idx
        Skip   next -> loop next idx
        Emit b next -> [|| do
            P.writePrimArray $$arr $$idx $$b
            $$(loop next [|| $$idx + 1 ||])
         ||]
