{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}
-- {-# OPTIONS_GHC -ddump-splices -dsuppress-module-prefixes #-}
module Staged.GHC.Generics.Instances () where

import Staged.GHC.Generics.TH

import Data.Functor.Compose  (Compose (..))
import Data.Functor.Const    (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product  (Product (..))
import Data.Functor.Sum      (Sum (..))

-------------------------------------------------------------------------------
-- Base
-------------------------------------------------------------------------------

$(deriveGeneric ''())

$(deriveGeneric ''Ordering)
$(deriveGeneric ''(,))

$(deriveGeneric  ''Maybe)
$(deriveGeneric1 ''Maybe)

$(deriveGeneric  ''[])
$(deriveGeneric1 ''[])

-------------------------------------------------------------------------------
-- Data.Functor.*
-------------------------------------------------------------------------------

$(deriveGeneric ''Identity)
$(deriveGeneric1 ''Identity)

$(deriveGeneric ''Const)
$(deriveGeneric1 ''Const)

$(deriveGeneric ''Sum)
$(deriveGeneric1 ''Sum)

$(deriveGeneric ''Product)
$(deriveGeneric1 ''Product)

$(deriveGeneric ''Compose)
$(deriveGeneric1 ''Compose)
