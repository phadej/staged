{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Staged.Stream.Final.States where

import Data.Monoid (Ap (..))
import Data.Proxy  (Proxy (..))
import Data.SOP    (SListI2, SOP (..))

import qualified GHC.Generics as GHC

import Data.SOP.Fn.Append
import Data.SOP.Fn.ConcatMapAppend

data One code
    = One
  deriving (GHC.Generic)

data Two code
    = Two0
    | Two1
  deriving (GHC.Generic)

-- | For @>>>@ combinator
data Comp xss yss code
    = CompL (SOP code xss)
    | CompR (SOP code xss) (SOP code yss)
  deriving (GHC.Generic)

-- | For @append@ combinator
data App a xss yss code
    = AppL (Ap code a) (SOP code xss)
    | AppR             (SOP code yss)
  deriving (GHC.Generic)

-- | For @zipWith@ combinator
data Zip a xss yss code
    = ZipL          (SOP code xss) (SOP code yss)
    | ZipR (code a) (SOP code xss) (SOP code yss)
  deriving (GHC.Generic)

-- | For @alignWith" combinator
data Align a xss yss code
    = AlignL               (SOP code xss) (SOP code yss)
    | AlignR      (code a) (SOP code xss) (SOP code yss)
    | AlignDrainL          (SOP code xss)
    | AlignDrainR                         (SOP code yss)
  deriving (GHC.Generic)

-- | For @bfsTree@ combinator
data BFS a xss code
    = BfsNext            (SOP code xss)
    | BfsStep (code [a]) (SOP code xss)
  deriving (GHC.Generic)

data Drop xss code
    = DropL (code Int) (SOP code xss)
    | DropR            (SOP code xss)
  deriving (GHC.Generic)

-------------------------------------------------------------------------------
-- Composition
-------------------------------------------------------------------------------

comp_SOP
    :: forall code xss yss. (SListI2 xss, SListI2 yss)
    => Comp xss yss code
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
    -> Comp xss yss code
uncomp_SOP (SOP sop) = case split_NS' prXss (prConcatMapAppend prXss prYss) sop of
    Left xss   -> CompL (SOP xss)
    Right sop' -> case unconcatMapAppend_NSNP sop' of
        (xss, yss) -> CompR (SOP xss) (SOP yss)
  where
    prXss = Proxy :: Proxy xss
    prYss = Proxy :: Proxy yss
