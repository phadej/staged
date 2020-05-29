{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Staged.Stream.States where

import Data.Proxy     (Proxy (..))
import Data.SOP   (SListI2, SOP (..))
import Staged.Commons (C)

import qualified GHC.Generics as GHC

import Data.SOP.Fn.Append
import Data.SOP.Fn.ConcatMapAppend


-- | For @>>>@ combinator
data Comp xss yss
    = CompL (SOP C xss)
    | CompR (SOP C xss) (SOP C yss)
  deriving (GHC.Generic)

-- | For @append@ combinator
data App a xss yss
    = AppL (C a) (SOP C xss)
    | AppR       (SOP C yss)
  deriving (GHC.Generic)

-- | For @zipWith@ combinator
data Zip a xss yss
    = ZipL       (SOP C xss) (SOP C yss)
    | ZipR (C a) (SOP C xss) (SOP C yss)
  deriving (GHC.Generic)

-- | For @alignWith" combinator
data Align a xss yss
    = AlignL            (SOP C xss) (SOP C yss)
    | AlignR      (C a) (SOP C xss) (SOP C yss)
    | AlignDrainL       (SOP C xss)
    | AlignDrainR                   (SOP C yss)
  deriving (GHC.Generic)

-- | For @bfsTree@ combinator
data BFS a xss
    = BfsNext         (SOP C xss)
    | BfsStep (C [a]) (SOP C xss)
  deriving (GHC.Generic)

data Drop xss
    = DropL (C Int) (SOP C xss)
    | DropR         (SOP C xss)
  deriving (GHC.Generic)

-------------------------------------------------------------------------------
-- Composition
-------------------------------------------------------------------------------

comp_SOP
    :: forall xss yss. (SListI2 xss, SListI2 yss)
    => Comp xss yss
    -> SOP C (Append xss (ConcatMapAppend xss yss))
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
    :: forall xss yss. (SListI2 xss, SListI2 yss)
    => SOP C (Append xss (ConcatMapAppend xss yss))
    -> Comp xss yss
uncomp_SOP (SOP sop) = case split_NS' prXss (prConcatMapAppend prXss prYss) sop of
    Left xss   -> CompL (SOP xss)
    Right sop' -> case unconcatMapAppend_NSNP sop' of
        (xss, yss) -> CompR (SOP xss) (SOP yss)
  where
    prXss = Proxy :: Proxy xss
    prYss = Proxy :: Proxy yss
